let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let inhibit fn = try fn () with _exn -> ()
let hdr_max_ns = 1_000_000_000

let clamp_dur dur64 =
  if Int64.compare dur64 (Int64.of_int hdr_max_ns) >= 0 then hdr_max_ns
  else if Int64.compare dur64 0L <= 0 then 0
  else Int64.to_int dur64

let mark_ready (task : Task.t) ts = task.ready_at <- ts

let record_scheduled (task : Task.t) ts =
  if Int64.compare task.ready_at 0L > 0 then begin
    let dur64 = Int64.sub ts task.ready_at in
    let dur = clamp_dur dur64 in
    if dur > 0 then begin
      ignore (Hdr_histogram.record_value task.scheduled_histogram dur);
      task.scheduled_count <- task.scheduled_count + 1;
      task.total_scheduled_ns <- Int64.add task.total_scheduled_ns dur64
    end;
    task.ready_at <- 0L
  end

let record_busy (task : Task.t) ts =
  if Int64.compare task.start_at 0L > 0 then begin
    let dur64 = Int64.sub ts task.start_at in
    let dur = clamp_dur dur64 in
    if dur > 0 then ignore (Hdr_histogram.record_value task.poll_histogram dur);
    task.total_busy_ns <- Int64.add task.total_busy_ns dur64;
    task.count <- task.count + 1
  end

(* When the fiber actively transitioned to a wait state during its run (Suspend,
   Await, Yield), Run_end must not clobber that state back to Idle. *)
let is_waiting = function
  | State.Suspended _ | State.Awaiting | State.Yielded -> true
  | _ -> false

(* Ring-local events (Run_begin/end, Await, Yield, Suspend, Cancelled, Run_done)
   come from the ring that owns the task and are therefore ordered per task.
   Resume is emitted from the signalling ring, which may differ, so it can
   arrive with a timestamp older than the last ring-local event — a stale
   Resume referring to an earlier await cycle that has already been consumed.
   We skip state transitions and ready marking for such stale Resumes. *)
let touch_last (task : Task.t) ts =
  if Int64.compare ts task.Task.last_ts > 0 then task.Task.last_ts <- ts

let fn g ring_id ts event =
  let ts = Runtime_events.Timestamp.to_int64 ts in
  let wall_ns = Domain.now_wall_ns () in
  g.G.counter <- g.G.counter + 1;
  (* Events from a runner ring ([Run_{begin,end,done}, [Suspend], [Continue]...) can be
     read before the [Spawn] for the same uid on the parent's ring, so we
     materialise the task on first sight rather than dropping. [Spawn] arriving
     later only fills in identity fields. *)
  let with_task uid f = f (Tree.task g.G.tree uid) in
  match event with
  | Miou.Trace.Spawn { uid; parent; runner; kind } ->
      let task = Tree.task g.tree uid in
      task.Task.parent <- parent;
      task.Task.runner <- runner;
      task.Task.kind <- kind;
      mark_ready task ts
  | Miou.Trace.Spawn_location { uid; filename; line } ->
      with_task uid @@ fun task -> task.Task.location <- Some (filename, line)
  | Miou.Trace.Run_begin uid ->
      with_task uid @@ fun task ->
      record_scheduled task ts;
      task.Task.state <- State.Running;
      task.Task.start_at <- ts;
      task.Task.run_mark <- Unix.gettimeofday ();
      touch_last task ts;
      let domain = Domains.get g.G.domains ring_id in
      Domain.accrue domain ~now:wall_ns;
      domain.Domain.is_active <- true;
      domain.Domain.active_task <- Some uid
  | Miou.Trace.Run_end uid ->
      with_task uid @@ fun task ->
      record_busy task ts;
      if not (is_waiting task.Task.state) then task.Task.state <- State.Idle;
      touch_last task ts;
      let domain = Domains.get g.domains ring_id in
      Domain.accrue domain ~now:wall_ns;
      domain.Domain.is_active <- false;
      domain.Domain.active_task <- None
  | Miou.Trace.Await uid ->
      with_task uid @@ fun task ->
      (* Miou emits Suspend (syscall name) followed by an internal Await on
         the trigger; preserve the more informative Suspended state. *)
      begin match task.Task.state with
      | State.Suspended _ -> ()
      | _ -> task.Task.state <- State.Awaiting
      end;
      touch_last task ts
  | Miou.Trace.Resume uid ->
      with_task uid @@ fun task ->
      task.Task.wakes <- task.Task.wakes + 1;
      (* Skip state/ready transitions for a stale Resume whose timestamp
         predates the task's most recent ring-local event. *)
      if Int64.compare ts task.Task.last_ts >= 0 then begin
        if is_waiting task.Task.state then begin
          task.Task.state <- State.Waking;
          task.Task.wake_mark <- Unix.gettimeofday ()
        end;
        mark_ready task ts
      end
  | Miou.Trace.Yield uid ->
      with_task uid @@ fun task ->
      task.Task.yields <- task.Task.yields + 1;
      task.Task.state <- State.Yielded;
      task.Task.yield_mark <- Unix.gettimeofday ();
      mark_ready task ts;
      touch_last task ts
  | Miou.Trace.Suspend { name; uid } ->
      with_task uid @@ fun task ->
      task.Task.state <- State.Suspended name;
      task.Task.suspend_start <- ts;
      task.Task.suspends <- task.Task.suspends + 1;
      touch_last task ts
  | Miou.Trace.Continue { name= _; uid } ->
      with_task uid @@ fun task ->
      if Int64.compare task.Task.suspend_start 0L > 0 then begin
        let delta = Int64.sub ts task.Task.suspend_start in
        if Int64.compare delta task.Task.max_blocking_ns > 0 then
          task.Task.max_blocking_ns <- delta;
        task.Task.suspend_start <- 0L
      end;
      touch_last task ts
  | Miou.Trace.Attach { ruid; puid } ->
      with_task puid @@ fun task ->
      if not (List.mem ruid task.Task.resources) then
        task.Task.resources <- ruid :: task.Task.resources
  | Miou.Trace.Detach { ruid; puid } ->
      with_task puid @@ fun task ->
      task.Task.resources <-
        List.filter (fun r -> r <> ruid) task.Task.resources
  | Miou.Trace.Cancelled uid ->
      with_task uid @@ fun task ->
      task.Task.state <- State.Cancelled;
      task.Task.finish_mark <- Unix.gettimeofday ();
      touch_last task ts
  | Miou.Trace.Run_done uid ->
      with_task uid @@ fun task ->
      task.Task.state <- State.Finished;
      task.Task.finish_mark <- Unix.gettimeofday ();
      touch_last task ts
  | _ -> ()

(* Closest still-alive uid to [uid] in [prior] - prefer successors, fall
   back to predecessors. Used when the selected task gets swept. *)
let nearest_alive tree uid prior =
  let alive u = Hashtbl.mem tree u in
  let rec after = function
    | [] -> None
    | x :: rest when x = uid -> List.find_opt alive rest
    | _ :: rest -> after rest
  in
  let rec before best = function
    | [] -> best
    | x :: _ when x = uid -> best
    | x :: rest -> before (if alive x then Some x else best) rest
  in
  match after prior with Some _ as s -> s | None -> before None prior

(* After a sweep, if the selected uid got evicted, move the cursor to a
   neighbour and drop out of [Detail] view if it pointed at that uid. *)
let reselect_if_evicted (r : G.react) (g : G.t) prior =
  match Lwd.peek r.selected with
  | Some uid when not (Hashtbl.mem g.G.tree uid) -> (
      Lwd.set r.selected (nearest_alive g.G.tree uid prior);
      match Lwd.peek r.ui_mode with
      | G.Detail u when u = uid -> Lwd.set r.ui_mode G.Tree
      | _ -> ())
  | _ -> ()

let run program runtime_dir =
  Miou_unix.run ~domains:1 @@ fun () ->
  let pid, child_cleanup =
    match program with
    | `Pid pid -> (pid, fun () -> ())
    | `Exec argv ->
        let null = Unix.openfile "/dev/null" Unix.[ O_RDWR; O_CLOEXEC ] 0 in
        let stdin = null and stdout = null and stderr = null in
        let pid = Mtbox.spawn ~stdin ~stdout ~stderr ~runtime_dir argv in
        Unix.close null;
        let reaped = Atomic.make false in
        let on_sigchld _ =
          match Unix.waitpid [ Unix.WNOHANG ] pid with
          | 0, _ -> ()
          | _, _ -> Atomic.set reaped true
          | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
              Atomic.set reaped true
        in
        let prev = Miou.sys_signal Sys.sigchld (Sys.Signal_handle on_sigchld) in
        let cleanup () =
          ignore (Miou.sys_signal Sys.sigchld prev);
          if not (Atomic.get reaped) then begin
            inhibit (fun () -> Unix.kill pid Sys.sigterm);
            inhibit (fun () -> ignore (Unix.waitpid [] pid))
          end
        in
        (pid, cleanup)
  in
  let r = G.r () and g = G.create () in
  let queue = Miou.Queue.create () in
  let lost = Atomic.make 0 in
  let reader =
    Miou.call @@ fun () ->
    let cursor = Runtime_events.create_cursor (Some (runtime_dir, pid)) in
    let lost_events _ring_id count = ignore (Atomic.fetch_and_add lost count) in
    let cbs = Runtime_events.Callbacks.create ~lost_events () in
    let push ring_id ts event = Miou.Queue.enqueue queue (ring_id, ts, event) in
    let cbs = Miou_runtime_events.add_callbacks ~fn:push cbs in
    let finally () = Runtime_events.free_cursor cursor in
    Fun.protect ~finally @@ fun () ->
    let rec go () =
      let _n = Runtime_events.read_poll cursor cbs None in
      Miou_unix.sleep 0.05; go ()
    in
    go ()
  in
  let sweep_grace_s = 0.3 in
  let updater =
    Miou.async @@ fun () ->
    let rec go tick =
      (* While paused, leave events queued. On unpause the next tick drains
         the full backlog at once, which re-syncs the tree (Suspended tasks
         that were resumed during the pause transition to Running, etc.). *)
      if not g.paused then begin
        let events = Miou.Queue.(to_list (transfer queue)) in
        g.lost <- Atomic.get lost;
        List.iter (fun (ring_id, ts, event) -> fn g ring_id ts event) events;
        G.notify r
      end;
      Domains.tick ~now:(Domain.now_wall_ns ()) g.G.domains;
      if tick mod 2 = 0 then begin
        let prior = Tree.visible_order g.G.tree in
        let keep =
          match Lwd.peek r.G.selected with
          | Some uid -> ( = ) uid
          | None -> fun _ -> false
        in
        let _ =
          Tree.sweep g.G.tree ~keep ~paused:g.paused ~now:(Unix.gettimeofday ())
            ~max_age_s:sweep_grace_s
        in
        reselect_if_evicted r g prior
      end;
      Miou_unix.sleep 0.1;
      go (tick + 1)
    in
    go 0
  in
  Nottui_miou.run (View.root r g);
  Miou.cancel updater;
  Miou.cancel reader;
  child_cleanup ()

open Cmdliner

let pid =
  let doc = "PID of the target Miou process to trace." in
  let open Arg in
  required & opt (some int) None & info [ "p"; "pid" ] ~doc ~docv:"PID"

let runtime_dir =
  let doc = "Path to the directory containing runtime_events files." in
  let env = Cmd.Env.info "OCAML_RUNTIME_EVENT_DIR" ~doc in
  let directory =
    let parser = function
      | str when Sys.file_exists str && Sys.is_directory str -> Ok str
      | str -> error_msgf "Directory %s does not exist" str
    in
    let pp = Fmt.string in
    Arg.conv (parser, pp)
  in
  let temp = Filename.get_temp_dir_name () in
  let open Arg in
  value & opt directory temp & info [ "runtime-dir" ] ~env ~doc ~docv:"DIR"

let term exec =
  let open Term in
  match exec with
  | Some exec -> const run $ const (`Exec exec) $ runtime_dir
  | None -> const run $ map (fun pid -> `Pid pid) pid $ runtime_dir

let cmd exec =
  let doc = "Live TUI monitor for Miou applications." in
  let info = Cmd.info "mtop" ~doc in
  Cmd.v info (term exec)

let () =
  match Mtbox.split_argv Sys.argv with
  | _argv, [] -> Cmd.(exit @@ eval (cmd None))
  | argv, exec -> Cmd.(exit @@ eval ~argv (cmd (Some exec)))
