(* Background watcher for Miou structured-concurrency violations.

   Listens to a target process's runtime_events stream and reports on the
   four diagnostic events Miou emits when the program breaks an invariant:
   + Still_has_children: a task finished while it still had unawaited children
     (structured concurrency violation).
   + Not_a_child: a task awaited a promise that isn't its direct child.
     Resource_leaked: an ownership-tracked resource was not disowned before its
     holder finished.
   + Not_owner: a task tried to use a resource owned by another task.
   + Resource_leaked: a resource was not disown by a task.

   Memory footprint is bounded by evicting terminal tasks after a short
   grace window - long enough that error events referring to a task can
   still retrieve its identity. *)

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let inhibit fn = try fn () with _exn -> ()

type task = {
    uid: int
  ; mutable parent: int
  ; mutable runner: int
  ; mutable location: (string * int) option
  ; mutable alive: bool
  ; mutable cancelled: bool
  ; mutable cleaned: bool
  ; mutable terminal_wall: float
  ; mutable resources: int list
}

let tasks : (int, task) Hashtbl.t = Hashtbl.create 1024
let owners : (int, int) Hashtbl.t = Hashtbl.create 1024

let get uid =
  match Hashtbl.find_opt tasks uid with
  | Some t -> t
  | None ->
      let t =
        {
          uid
        ; parent= -1
        ; runner= 0
        ; location= None
        ; alive= true
        ; cancelled= false
        ; cleaned= false
        ; terminal_wall= 0.0
        ; resources= []
        }
      in
      Hashtbl.replace tasks uid t;
      t

let location_to_string t =
  match t.location with
  | Some (f, l) -> Fmt.str "%s:%d" f l
  | None -> "<unknown>"

let short uid =
  match Hashtbl.find_opt tasks uid with
  | None -> Fmt.str "#%d <evicted>" uid
  | Some t -> Fmt.str "#%d %s" uid (location_to_string t)

let now_str () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Fmt.str "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let parent_chain uid =
  let rec loop uid acc depth =
    if depth > 32 then List.rev acc
    else
      match Hashtbl.find_opt tasks uid with
      | None -> List.rev acc
      | Some t ->
          let acc = uid :: acc in
          if t.parent < 0 || t.parent = uid then List.rev acc
          else loop t.parent acc (depth + 1)
  in
  loop uid [] 0

let children_of uid =
  let acc = ref [] in
  let fn _ t = if t.parent = uid && not t.cleaned then acc := t :: !acc in
  Hashtbl.iter fn tasks;
  List.sort (fun a b -> Int.compare a.uid b.uid) !acc

let state_to_string t =
  if t.alive then "running"
  else if t.cancelled then "cancelled"
  else "terminated"

let print_report severity name lines =
  let attr = match severity with `Warn -> "WARN" | `Error -> "ERROR" in
  Fmt.pr "[%s] %-5s %-20s\n" (now_str ()) attr name;
  List.iter (fun l -> Fmt.pr "        %s\n" l) lines;
  Fmt.pr "%!"

let report_still_has_children uid =
  let _ = get uid in
  let kids = children_of uid in
  let kid_lines =
    let fn c =
      Fmt.str "- child %s (parent=#%d, runner=dom%d, %s)" (short c.uid) c.parent
        c.runner (state_to_string c)
    in
    List.map fn kids
  in
  let chain = parent_chain uid |> List.map (Fmt.str "#%d") in
  let chain_str = if chain = [] then "<root>" else String.concat " <- " chain in
  let header =
    Fmt.str
      "task %s finished while still holding %d unawaited child%s (fix: await \
       or cancel every child before returning)"
      (short uid) (List.length kids)
      (if List.length kids = 1 then "" else "ren")
  in
  let lines = [ header; Fmt.str "parent chain: %s" chain_str ] @ kid_lines in
  print_report `Error "Still_has_children" lines

let report_not_a_child self prm =
  let header =
    Fmt.str
      "task %s tried to await %s which is not its direct child (fix: only \
       await promises your task spawned)"
      (short self) (short prm)
  in
  let self_parent =
    match Hashtbl.find_opt tasks self with
    | Some t -> Fmt.str "self.parent=#%d" t.parent
    | None -> "self.parent=?"
  in
  let prm_parent =
    match Hashtbl.find_opt tasks prm with
    | Some t -> Fmt.str "target.parent=#%d" t.parent
    | None -> "target.parent=?"
  in
  print_report `Error "Not_a_child" [ header; self_parent; prm_parent ]

let report_resource_leaked uid =
  let t = get uid in
  let rs =
    match t.resources with
    | [] -> "<none tracked>"
    | rs -> String.concat ", " (List.map (fun r -> Fmt.str "#%d" r) rs)
  in
  let header =
    Fmt.str
      "task %s finished without disowning at least one resource (fix: call \
       Miou.Ownership.{disown,release} or transfer before return)"
      (short uid)
  in
  print_report `Error "Resource_leaked"
    [ header; Fmt.str "resources still attached: %s" rs ]

let report_not_owner ruid puid =
  let owner =
    match Hashtbl.find_opt owners ruid with
    | Some o -> Fmt.str "#%d" o
    | None -> "<unknown>"
  in
  let header =
    Fmt.str
      "task %s used resource #%d but does not own it (fix: use \
       Miou.Ownership.check from the owning task only)"
      (short puid) ruid
  in
  print_report `Error "Not_owner"
    [ header; Fmt.str "resource #%d owner=%s" ruid owner ]

let handle _ring_id _ts (event : Miou.Trace.event) =
  match event with
  | Miou.Trace.Spawn { uid; parent; runner; kind= _ } ->
      let t = get uid in
      t.parent <- parent;
      t.runner <- runner;
      t.alive <- true;
      t.cancelled <- false;
      t.cleaned <- false
  | Miou.Trace.Spawn_location { uid; filename; line } ->
      let t = get uid in
      t.location <- Some (filename, line)
  | Miou.Trace.Attach { ruid; puid } ->
      let t = get puid in
      if not (List.mem ruid t.resources) then t.resources <- ruid :: t.resources;
      Hashtbl.replace owners ruid puid
  | Miou.Trace.Detach { ruid; puid } -> (
      let t = get puid in
      t.resources <- List.filter (fun r -> r <> ruid) t.resources;
      match Hashtbl.find_opt owners ruid with
      | Some o when o = puid -> Hashtbl.remove owners ruid
      | _ -> ())
  | Miou.Trace.Run_done uid ->
      let t = get uid in
      t.alive <- false;
      t.terminal_wall <- Unix.gettimeofday ()
  | Miou.Trace.Cancelled uid ->
      let t = get uid in
      t.alive <- false;
      t.cancelled <- true;
      t.terminal_wall <- Unix.gettimeofday ()
  | Miou.Trace.Clean { self= _; child } ->
      let t = get child in
      t.cleaned <- true
  | Miou.Trace.Still_has_children uid -> report_still_has_children uid
  | Miou.Trace.Not_a_child { self; prm } -> report_not_a_child self prm
  | Miou.Trace.Resource_leaked uid -> report_resource_leaked uid
  | Miou.Trace.Not_owner { ruid; puid } -> report_not_owner ruid puid
  | _ -> ()

let sweep ~grace_s ~max_tasks =
  let now = Unix.gettimeofday () in
  let referenced = Hashtbl.create 0x3f in
  let fn _ t =
    if t.alive && t.parent >= 0 then Hashtbl.replace referenced t.parent ()
  in
  Hashtbl.iter fn tasks;
  let parent_is_alive t =
    match Hashtbl.find_opt tasks t.parent with
    | Some p -> p.alive
    | None -> false
  in
  let doomed = ref [] in
  let fn uid t =
    if
      (not t.alive)
      && t.terminal_wall > 0.0
      && now -. t.terminal_wall > grace_s
      && (not (Hashtbl.mem referenced uid))
      && (t.cleaned || not (parent_is_alive t))
    then doomed := uid :: !doomed
  in
  Hashtbl.iter fn tasks;
  List.iter (Hashtbl.remove tasks) !doomed;
  (* NOTE(dinosaure): if the table is still growing without bound (e.g. a flood
     of live tasks), drop the oldest terminal entries unconditionally. *)
  if Hashtbl.length tasks > max_tasks then begin
    let fn uid t acc =
      if not t.alive then (t.terminal_wall, uid) :: acc else acc
    in
    let victims = Hashtbl.fold fn tasks [] in
    let victims = List.sort (fun (a, _) (b, _) -> Float.compare a b) victims in
    let excess = Hashtbl.length tasks - max_tasks in
    let rec drop n = function
      | _ when n <= 0 -> ()
      | [] -> ()
      | (_, uid) :: rest ->
          Hashtbl.remove tasks uid;
          drop (n - 1) rest
    in
    drop excess victims
  end

let run program runtime_dir quiet =
  let keep_going = ref true in
  let stop _ = keep_going := false in
  Sys.set_signal Sys.sigint (Sys.Signal_handle stop);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle stop);
  let pid, child_cleanup =
    match program with
    | `Pid pid -> (pid, fun () -> ())
    | `Exec argv ->
        let pid = Mtbox.spawn ~runtime_dir argv in
        let cleaned = ref false in
        let on_sigchld _ =
          match Unix.waitpid [ Unix.WNOHANG ] pid with
          | 0, _ -> ()
          | _, _ ->
              cleaned := true;
              keep_going := false
          | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
              cleaned := true;
              keep_going := false
        in
        let prev = Sys.signal Sys.sigchld (Sys.Signal_handle on_sigchld) in
        let cleanup () =
          Sys.set_signal Sys.sigchld prev;
          if not !cleaned then begin
            inhibit (fun () -> Unix.kill pid Sys.sigterm);
            inhibit (fun () -> ignore (Unix.waitpid [] pid))
          end
        in
        (pid, cleanup)
  in
  if not quiet then Fmt.pr "diag: attached to pid=%d dir=%s\n%!" pid runtime_dir;
  let cursor = Runtime_events.create_cursor (Some (runtime_dir, pid)) in
  let lost = ref 0 in
  let lost_events _ n = lost := !lost + n in
  let cbs = Runtime_events.Callbacks.create ~lost_events () in
  let cbs = Miou_runtime_events.add_callbacks ~fn:handle cbs in
  let finally () =
    Runtime_events.free_cursor cursor;
    child_cleanup ()
  in
  let grace_s = 5.0 and max_tasks = 50_000 in
  let last_sweep = ref (Unix.gettimeofday ()) in
  let last_lost = ref 0 in
  Fun.protect ~finally @@ fun () ->
  while !keep_going do
    begin try ignore (Runtime_events.read_poll cursor cbs None)
    with _exn -> keep_going := false
    end;
    let now = Unix.gettimeofday () in
    if now -. !last_sweep > 1.0 then begin
      sweep ~grace_s ~max_tasks;
      last_sweep := now
    end;
    if !lost > !last_lost then begin
      if not quiet then
        Fmt.pr "[%s] LOST  runtime_events dropped %d events\n%!" (now_str ())
          (!lost - !last_lost);
      last_lost := !lost
    end;
    Unix.sleepf 0.1
  done;
  (* Drain any final events emitted just before the child exited. *)
  inhibit (fun () -> ignore (Runtime_events.read_poll cursor cbs None));
  if not quiet then
    Fmt.pr "diag: exit (tasks seen=%d, lost=%d)\n%!" (Hashtbl.length tasks)
      !lost

open Cmdliner

let pid =
  let doc = "PID of the target Miou process to watch." in
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

let quiet =
  let doc = "Suppress startup and lost-events notices (errors still print)." in
  let open Arg in
  value & flag & info [ "q"; "quiet" ] ~doc

let term exec =
  let open Term in
  match exec with
  | Some exec -> const run $ const (`Exec exec) $ runtime_dir $ quiet
  | None -> const run $ map (fun pid -> `Pid pid) pid $ runtime_dir $ quiet

let cmd exec =
  let doc =
    "Background watcher that reports Miou structured-concurrency violations."
  in
  let info = Cmd.info "diag" ~doc in
  Cmd.v info (term exec)

let () =
  match Mtbox.split_argv Sys.argv with
  | _argv, [] -> Cmd.(exit @@ eval (cmd None))
  | argv, exec -> Cmd.(exit @@ eval ~argv (cmd (Some exec)))
