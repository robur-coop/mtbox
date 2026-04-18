let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module Phase = struct
  type t = Begin | End | Instant

  let to_char = function Begin -> 'B' | End -> 'E' | Instant -> 'i'
end

module Event = struct
  type t = {
      name: string
    ; cat: string
    ; ph: Phase.t
    ; ts: float
    ; pid: int
    ; tid: int
    ; args: (string * string) list
  }

  let v ?(cat = "miou") ~ph ~ts ~pid ~tid ?(args = []) name =
    { name; cat; ph; ts; pid; tid; args }

  let pp_string ppf str =
    for idx = 0 to String.length str - 1 do
      match str.[idx] with
      | '"' -> Fmt.pf ppf "\\\""
      | '\\' -> Fmt.pf ppf "\\\\"
      | '\n' -> Fmt.pf ppf "\\\n"
      | '\r' -> Fmt.pf ppf "\\\r"
      | '\t' -> Fmt.pf ppf "\\\t"
      | chr -> Fmt.pf ppf "%c" chr
    done

  let pp_scope ppf = function
    | Phase.Instant -> Fmt.pf ppf {json|,"s":"t"|json}
    | _ -> ()

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
      {json|{"name":"%a","cat":"%a","ph":"%c","ts":%.3f,"pid":%d,"tid":%d%a%a}|json}
      pp_string ev.name pp_string ev.cat (Phase.to_char ev.ph) ev.ts ev.pid
      ev.tid pp_scope ev.ph pp_args ev.args

  let from_runtime_event ~pid ring_id ts (ev : Miou.Trace.event) =
    let ts = ts_of_timestamp ts in
    let tid = ring_id_of_domain_id ring_id in
    match ev with
    | Miou.Trace.Spawn { uid; parent; runner; kind } ->
        let kind =
          match kind with `Async -> "async" | `Parallel -> "parallel"
        in
        let name = Fmt.str "spawn:%d" uid in
        let args =
          [
            ("parent", string_of_int parent); ("kind", kind)
          ; ("runner", string_of_int runner)
          ]
        in
        v ~ph:Phase.Instant ~ts ~pid ~tid ~args name
    | Miou.Trace.Spawn_location { uid; filename; line } ->
        let name = Fmt.str "location:%d" uid in
        let cat = "miou.meta" in
        let args = [ ("filename", filename); ("line", string_of_int line) ] in
        v ~ph:Phase.Instant ~cat ~ts ~pid ~tid ~args name
    | Miou.Trace.Run_begin uid ->
        let name = Fmt.str "task:%d" uid in
        v ~ph:Phase.Begin ~ts ~pid ~tid name
    | Miou.Trace.Run_end uid ->
        let name = Fmt.str "task:%d" uid in
        v ~ph:Phase.End ~ts ~pid ~tid name
    | Miou.Trace.Run_done uid ->
        let name = Fmt.str "done:%d" uid in
        v ~ph:Phase.Instant ~ts ~pid ~tid name
    | Miou.Trace.Cancel uid ->
        (* TODO(dinosaure): use [Begin]/[End] (with Cancelled)? *)
        let name = Fmt.str "cancel:%d" uid in
        v ~ph:Phase.Instant ~ts ~pid ~tid name
    | Miou.Trace.Cancelled uid ->
        let name = Fmt.str "cancelled:%d" uid in
        v ~ph:Phase.Instant ~ts ~pid ~tid name
    | Miou.Trace.Await uid ->
        let name = Fmt.str "await:%d" uid in
        v ~ph:Phase.Instant ~ts ~pid ~tid name
    | Miou.Trace.Resume uid ->
        let name = Fmt.str "resume:%d" uid in
        v ~ph:Phase.Instant ~ts ~pid ~tid name
    | Miou.Trace.Yield uid ->
        let name = Fmt.str "yield:%d" uid in
        v ~ph:Phase.Instant ~ts ~pid ~tid name
    | Miou.Trace.Suspend { uid; name= syscall } ->
        let name = Fmt.str "suspend:%d" uid in
        let args = [ ("syscall", syscall) ] in
        (* TODO(dinosaure): use [b] and [e] (async events)? *)
        v ~ph:Phase.Begin ~ts ~pid ~tid ~args name
    | Miou.Trace.Continue { uid; name= syscall } ->
        let name = Fmt.str "unblock:%d" uid in
        let args = [ ("syscall", syscall) ] in
        v ~ph:Phase.End ~ts ~pid ~tid ~args name
    | Miou.Trace.Attach { ruid; puid } ->
        let name = Fmt.str "attach:%d" ruid in
        let args =
          [ ("task", string_of_int puid); ("resource", string_of_int ruid) ]
        in
        let cat = "miou.resources" in
        v ~cat ~ph:Phase.Begin ~ts ~pid ~tid ~args name
    | Miou.Trace.Detach { ruid; puid } ->
        let name = Fmt.str "detach:%d" ruid in
        let args =
          [ ("task", string_of_int puid); ("resource", string_of_int ruid) ]
        in
        let cat = "miou.resources" in
        v ~cat ~ph:Phase.End ~ts ~pid ~tid ~args name
    | Miou.Trace.Still_has_children uid ->
        let name = Fmt.str "error:still_has_children:%d" uid in
        let cat = "miou.error" in
        v ~cat ~ph:Phase.Instant ~ts ~pid ~tid name
    | Miou.Trace.Not_a_child { self; prm } ->
        let name = Fmt.str "error:not_a_child:%d:%d" self prm in
        let cat = "miou.error" in
        v ~cat ~ph:Phase.Instant ~ts ~pid ~tid name
    | _ -> failwith "Unhandled event"
end

exception Exit

let poll ?duration stop (path, pid) counter queue =
  let cursor = Runtime_events.create_cursor (Some (path, pid)) in
  let finally = Runtime_events.free_cursor in
  let res = Miou.Ownership.create ~finally cursor in
  Miou.Ownership.own res;
  let cbs = Runtime_events.Callbacks.create () in
  let fn ring_id ts ev =
    let ev = Event.from_runtime_event ~pid ring_id ts ev in
    Atomic.incr counter;
    Miou.Queue.enqueue queue ev
  in
  let cbs = Miou_runtime_events.add_callbacks ~fn cbs in
  let start = Unix.gettimeofday () in
  let rec go () =
    if Atomic.get stop then raise Exit;
    begin match duration with
    | Some duration when Unix.gettimeofday () -. start >= duration -> raise Exit
    | _ -> ()
    end;
    let _n = Runtime_events.read_poll cursor cbs (Some 1000) in
    Miou_unix.sleep 0.01; go ()
  in
  go ()

let emit stop output queue =
  let oc, finally =
    match output with
    | None -> (stdout, ignore)
    | Some filepath ->
        let oc = open_out_bin filepath in
        let finally = close_out in
        (oc, finally)
  in
  let finally ppf = Fmt.pf ppf "\n]}\n%!"; finally oc in
  let ppf = Format.formatter_of_out_channel oc in
  Fmt.pf ppf "{\"traceEvents\":[\n";
  let res = Miou.Ownership.create ~finally ppf in
  Miou.Ownership.own res;
  let rec go () =
    if Atomic.get stop then raise Exit;
    let local = Miou.Queue.transfer queue in
    let events = Miou.Queue.to_list local in
    List.iter (Event.pp ppf) events;
    go (Miou.yield ())
  in
  go ()

let run quiet pid output duration runtime_dir =
  let domains = Int.min 1 (Domain.recommended_domain_count () - 1) in
  Miou_unix.run ~domains @@ fun () ->
  let call fn = if domains >= 1 then Miou.call fn else Miou.async fn in
  let stop = Atomic.make false in
  let behavior = Sys.Signal_handle (fun _ -> Atomic.set stop true) in
  let _ = Miou.sys_signal Sys.sigint behavior in
  let counter = Atomic.make 0 in
  let queue = Miou.Queue.create () in
  let prm0 =
    call @@ fun () -> poll ?duration stop (runtime_dir, pid) counter queue
  in
  let prm1 = Miou.async @@ fun () -> emit stop output queue in
  let _ = Miou.await_all [ prm0; prm1 ] in
  if not quiet then Fmt.pr "%d event(s) recorded\n%!" (Atomic.get counter)

open Cmdliner
open Mtbox_cli

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

let term =
  let open Term in
  const run $ setup_logs $ pid $ output $ duration $ runtime_dir

let cmd =
  let doc =
    "Record Miou runtime events to a Chrome Trace Event Format JSON file."
  in
  let info = Cmd.info "recd" ~doc in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
