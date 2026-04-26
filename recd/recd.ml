let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let inhibit fn = try fn () with _exn -> ()

module Phase = struct
  type t = Begin | End | Instant | Async_begin | Async_end | Metadata

  let to_char = function
    | Begin -> 'B'
    | End -> 'E'
    | Instant -> 'i'
    | Async_begin -> 'b'
    | Async_end -> 'e'
    | Metadata -> 'M'
end

module Event = struct
  type t = {
      name: string
    ; cat: string
    ; ph: Phase.t
    ; ts: float
    ; pid: int
    ; tid: int
    ; id: string option
    ; args: (string * string) list
  }

  let v ?(cat = "miou") ~ph ~ts ~pid ~tid ?id ?(args = []) name =
    { name; cat; ph; ts; pid; tid; id; args }

  let pp_string ppf str =
    for idx = 0 to String.length str - 1 do
      match str.[idx] with
      | '"' -> Fmt.pf ppf "\\\""
      | '\\' -> Fmt.pf ppf "\\\\"
      | '\n' -> Fmt.pf ppf "\\\\n"
      | '\r' -> Fmt.pf ppf "\\\\r"
      | '\t' -> Fmt.pf ppf "\\\\t"
      | chr when Char.code chr < 0x20 -> Fmt.pf ppf "\\u%04x" (Char.code chr)
      | chr -> Fmt.pf ppf "%c" chr
    done

  let pp_scope ppf = function
    | Phase.Instant -> Fmt.pf ppf {json|,"s":"t"|json}
    | _ -> ()

  let pp_id ppf = function
    | None -> ()
    | Some id -> Fmt.pf ppf {json|,"id":"%a"|json} pp_string id

  let pp_args ppf = function
    | [] -> ()
    | hd :: tl ->
        Fmt.pf ppf {json|,"args":{"%a":"%a"|json} pp_string (fst hd) pp_string
          (snd hd);
        let fn (k, v) =
          Fmt.pf ppf {json|,"%a":"%a"|json} pp_string k pp_string v
        in
        List.iter fn tl; Fmt.pf ppf {json|}|json}

  let base = ref 0L

  let ts_of_timestamp ts =
    let ns = Runtime_events.Timestamp.to_int64 ts in
    if Int64.equal !base 0L then base := ns;
    Int64.to_float (Int64.sub ns !base) /. 1000.0

  let ring_id_of_domain_id = Fun.id

  let pp ppf ev =
    Fmt.pf ppf
      {json|{"name":"%a","cat":"%a","ph":"%c","ts":%.3f,"pid":%d,"tid":%d%a%a%a}|json}
      pp_string ev.name pp_string ev.cat (Phase.to_char ev.ph) ev.ts ev.pid
      ev.tid pp_scope ev.ph pp_id ev.id pp_args ev.args

  let process_name ~pid =
    let args = [ ("name", Fmt.str "miou pid %d" pid) ] in
    {
      name= "process_name"
    ; cat= "__metadata"
    ; ph= Phase.Metadata
    ; ts= 0.0
    ; pid
    ; tid= 0
    ; id= None
    ; args
    }

  let thread_name ~pid ~tid =
    let args = [ ("name", Fmt.str "Domain %d" tid) ] in
    {
      name= "thread_name"
    ; cat= "__metadata"
    ; ph= Phase.Metadata
    ; ts= 0.0
    ; pid
    ; tid
    ; id= None
    ; args
    }

  let from_runtime_event ~pid ring_id ts (ev : Miou.Trace.event) =
    let ts = ts_of_timestamp ts in
    let tid = ring_id_of_domain_id ring_id in
    match ev with
    | Miou.Trace.Spawn { uid; parent; runner; kind } ->
        let kind =
          match kind with `Async -> "async" | `Parallel -> "parallel"
        in
        let name = Fmt.str "task:%d" uid in
        let args =
          [
            ("parent", string_of_int parent); ("kind", kind)
          ; ("runner", string_of_int runner)
          ]
        in
        let id = string_of_int uid in
        [
          v ~cat:"miou.task" ~ph:Phase.Async_begin ~ts ~pid ~tid ~id ~args name
        ]
    | Miou.Trace.Spawn_location { uid; filename; line } ->
        let name = Fmt.str "location:%d" uid in
        let cat = "miou.meta" in
        let args = [ ("filename", filename); ("line", string_of_int line) ] in
        [ v ~ph:Phase.Instant ~cat ~ts ~pid ~tid ~args name ]
    | Miou.Trace.Run_begin uid ->
        let name = Fmt.str "task:%d" uid in
        [ v ~ph:Phase.Begin ~ts ~pid ~tid name ]
    | Miou.Trace.Run_end uid ->
        let name = Fmt.str "task:%d" uid in
        [ v ~ph:Phase.End ~ts ~pid ~tid name ]
    | Miou.Trace.Run_done uid ->
        let name = Fmt.str "task:%d" uid in
        let id = string_of_int uid in
        let done_name = Fmt.str "done:%d" uid in
        [
          v ~cat:"miou.task" ~ph:Phase.Async_end ~ts ~pid ~tid ~id name
        ; v ~ph:Phase.Instant ~ts ~pid ~tid done_name
        ]
    | Miou.Trace.Cancel uid ->
        let name = Fmt.str "cancel:%d" uid in
        [ v ~ph:Phase.Instant ~ts ~pid ~tid name ]
    | Miou.Trace.Cancelled uid ->
        let name = Fmt.str "cancelled:%d" uid in
        [ v ~ph:Phase.Instant ~ts ~pid ~tid name ]
    | Miou.Trace.Await uid ->
        let name = Fmt.str "await:%d" uid in
        [ v ~ph:Phase.Instant ~ts ~pid ~tid name ]
    | Miou.Trace.Resume uid ->
        let name = Fmt.str "resume:%d" uid in
        [ v ~ph:Phase.Instant ~ts ~pid ~tid name ]
    | Miou.Trace.Yield uid ->
        let name = Fmt.str "yield:%d" uid in
        [ v ~ph:Phase.Instant ~ts ~pid ~tid name ]
    | Miou.Trace.Suspend { uid; name= syscall } ->
        let name = Fmt.str "syscall:%s" syscall in
        let args = [ ("syscall", syscall); ("task", string_of_int uid) ] in
        let id = string_of_int uid in
        let cat = "miou.syscall" in
        [ v ~cat ~ph:Phase.Async_begin ~ts ~pid ~tid ~id ~args name ]
    | Miou.Trace.Continue { uid; name= syscall } ->
        let name = Fmt.str "syscall:%s" syscall in
        let args = [ ("syscall", syscall); ("task", string_of_int uid) ] in
        let id = string_of_int uid in
        let cat = "miou.syscall" in
        [ v ~cat ~ph:Phase.Async_end ~ts ~pid ~tid ~id ~args name ]
    | Miou.Trace.Attach { ruid; puid } ->
        let name = Fmt.str "resource:%d" ruid in
        let args =
          [ ("task", string_of_int puid); ("resource", string_of_int ruid) ]
        in
        let id = string_of_int ruid in
        let cat = "miou.resources" in
        [ v ~cat ~ph:Phase.Async_begin ~ts ~pid ~tid ~id ~args name ]
    | Miou.Trace.Detach { ruid; puid } ->
        let name = Fmt.str "resource:%d" ruid in
        let args =
          [ ("task", string_of_int puid); ("resource", string_of_int ruid) ]
        in
        let id = string_of_int ruid in
        let cat = "miou.resources" in
        [ v ~cat ~ph:Phase.Async_end ~ts ~pid ~tid ~id ~args name ]
    | Miou.Trace.Still_has_children uid ->
        let name = Fmt.str "error:still_has_children:%d" uid in
        let cat = "miou.error" in
        let args = [ ("task", string_of_int uid) ] in
        [ v ~cat ~ph:Phase.Instant ~ts ~pid ~tid ~args name ]
    | Miou.Trace.Not_a_child { self; prm } ->
        let name = Fmt.str "error:not_a_child:%d:%d" self prm in
        let cat = "miou.error" in
        let args =
          [ ("task", string_of_int self); ("promise", string_of_int prm) ]
        in
        [ v ~cat ~ph:Phase.Instant ~ts ~pid ~tid ~args name ]
    | Miou.Trace.Resource_leaked uid ->
        let name = Fmt.str "error:resource_leaked:%d" uid in
        let cat = "miou.error" in
        let args = [ ("task", string_of_int uid) ] in
        [ v ~cat ~ph:Phase.Instant ~ts ~pid ~tid ~args name ]
    | Miou.Trace.Not_owner { ruid; puid } ->
        let name = Fmt.str "error:not_owner:%d:%d" ruid puid in
        let cat = "miou.error" in
        let args =
          [ ("task", string_of_int puid); ("resource", string_of_int ruid) ]
        in
        [ v ~cat ~ph:Phase.Instant ~ts ~pid ~tid ~args name ]
    | _ ->
        let name = "unknown" in
        let cat = "miou.meta" in
        [ v ~cat ~ph:Phase.Instant ~ts ~pid ~tid name ]
end

let poll ?duration stop (path, pid) counter queue =
  let cursor = Runtime_events.create_cursor (Some (path, pid)) in
  let finally () = Runtime_events.free_cursor cursor in
  let cbs = Runtime_events.Callbacks.create () in
  let seen_tids = Hashtbl.create 16 in
  let fn ring_id ts ev =
    let tid = Event.ring_id_of_domain_id ring_id in
    if not (Hashtbl.mem seen_tids tid) then begin
      Hashtbl.add seen_tids tid ();
      Miou.Queue.enqueue queue (Event.thread_name ~pid ~tid);
      Atomic.incr counter
    end;
    let events = Event.from_runtime_event ~pid ring_id ts ev in
    let fn ev =
      Atomic.incr counter;
      Miou.Queue.enqueue queue ev
    in
    List.iter fn events
  in
  let cbs = Miou_runtime_events.add_callbacks ~fn cbs in
  let start = Unix.gettimeofday () in
  let loop () =
    let rec go () =
      if Atomic.get stop then ignore (Runtime_events.read_poll cursor cbs None)
      else
        match duration with
        | Some d when Unix.gettimeofday () -. start >= d -> Atomic.set stop true
        | _ ->
            ignore (Runtime_events.read_poll cursor cbs None);
            Miou_unix.sleep 0.01;
            go ()
    in
    go ()
  in
  Fun.protect ~finally loop

let emit stop pid output queue =
  let oc, close_oc =
    match output with
    | None -> (stdout, ignore)
    | Some filepath -> (open_out_bin filepath, close_out)
  in
  let ppf = Format.formatter_of_out_channel oc in
  let first = ref true in
  let finally () =
    Fmt.pf ppf "\n]}\n";
    Format.pp_print_flush ppf ();
    close_oc oc
  in
  let body () =
    Fmt.pf ppf "{\"displayTimeUnit\":\"us\",\"traceEvents\":[\n";
    let emit_one ev =
      if !first then first := false else Fmt.pf ppf ",\n";
      Event.pp ppf ev
    in
    emit_one (Event.process_name ~pid);
    let rec go () =
      let local = Miou.Queue.transfer queue in
      List.iter emit_one (Miou.Queue.to_list local);
      if Atomic.get stop then
        let local = Miou.Queue.transfer queue in
        List.iter emit_one (Miou.Queue.to_list local)
      else begin
        Miou_unix.sleep 0.05; go ()
      end
    in
    go ()
  in
  Fun.protect ~finally body

let run quiet program output duration runtime_dir =
  let domains = Int.min 1 (Domain.recommended_domain_count () - 1) in
  Miou_unix.run ~domains @@ fun () ->
  let call fn = if domains >= 1 then Miou.call fn else Miou.async fn in
  let stop = Atomic.make false in
  let behavior = Sys.Signal_handle (fun _ -> Atomic.set stop true) in
  let _ = Miou.sys_signal Sys.sigint behavior in
  let counter = Atomic.make 0 in
  let queue = Miou.Queue.create () in
  let prm0, pid =
    match program with
    | `Pid pid ->
        let prm =
          call @@ fun () -> poll ?duration stop (runtime_dir, pid) counter queue
        in
        (prm, pid)
    | `Exec argv ->
        let pid = Mtbox.spawn ~runtime_dir argv in
        let reaped = Atomic.make false in
        let fn _ =
          match Unix.waitpid [ Unix.WNOHANG ] pid with
          | 0, _ -> ()
          | _, _ -> Atomic.set reaped true; Atomic.set stop true
          | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
              Atomic.set reaped true; Atomic.set stop true
        in
        let handler = Sys.Signal_handle fn in
        let prev = Miou.sys_signal Sys.sigchld handler in
        let prm =
          call @@ fun () ->
          let finally () =
            ignore (Miou.sys_signal Sys.sigchld prev);
            if not (Atomic.get reaped) then begin
              inhibit (fun () -> Unix.kill pid Sys.sigterm);
              inhibit (fun () -> ignore (Unix.waitpid [] pid))
            end
          in
          Fun.protect ~finally @@ fun () ->
          poll ?duration stop (runtime_dir, pid) counter queue
        in
        (prm, pid)
  in
  let prm1 = Miou.async @@ fun () -> emit stop pid output queue in
  let _ = Miou.await_all [ prm0; prm1 ] in
  if not quiet then Fmt.pr "%d event(s) recorded\n%!" (Atomic.get counter)

open Cmdliner
open Mtbox

let pid =
  let doc = "PID of the target Miou process to trace." in
  let open Arg in
  required & opt (some int) None & info [ "p"; "pid" ] ~doc ~docv:"PID"

let output =
  let doc = "Output file path for the trace (JSON)." in
  let non_existing_filepath =
    let parser = function
      | "-" -> Ok None
      | str when Sys.file_exists str -> error_msgf "%s already exists" str
      | str -> Ok (Some str)
    in
    let pp ppf = function
      | None -> Fmt.string ppf "-"
      | Some str -> Fmt.string ppf str
    in
    Arg.conv (parser, pp)
  in
  let open Arg in
  value
  & opt non_existing_filepath None
  & info [ "o"; "output" ] ~doc ~docv:"FILE"

let duration =
  let doc = "Duration to record (default: until Ctrl-C)." in
  let duration =
    let parser str =
      match Duration.of_string str with
      | Ok duration -> Ok (Duration.to_f duration)
      | Error _ -> error_msgf "Invalid duration: %S" str
    in
    let pp ppf f = Duration.pp ppf (Duration.of_f f) in
    Arg.conv (parser, pp)
  in
  let open Arg in
  value
  & opt (some duration) None
  & info [ "d"; "duration" ] ~doc ~docv:"DURATION"

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
  | Some exec ->
      const run
      $ setup_logs
      $ const (`Exec exec)
      $ output
      $ duration
      $ runtime_dir
  | None ->
      const run
      $ setup_logs
      $ map (fun pid -> `Pid pid) pid
      $ output
      $ duration
      $ runtime_dir

let cmd exec =
  let doc =
    "Record Miou runtime events to a Chrome Trace Event Format JSON file."
  in
  let info = Cmd.info "recd" ~doc in
  Cmd.v info (term exec)

let () =
  match Mtbox.split_argv Sys.argv with
  | _argv, [] -> Cmd.(exit @@ eval (cmd None))
  | argv, exec -> Cmd.(exit @@ eval ~argv (cmd (Some exec)))
