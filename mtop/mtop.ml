let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module State = State
module Task = Task
module Tree = Tree
module Domain = Domain
module Domains = Domains
module Stats = Stats
module View = View
module G = G

let fn g ring_id ts event =
  let ts = Runtime_events.Timestamp.to_int64 ts in
  g.G.counter <- g.G.counter + 1;
  match event with
  | Miou.Trace.Spawn { uid; parent; runner; kind } ->
      let task = Tree.task g.tree uid in
      task.Task.parent <- parent;
      task.Task.runner <- runner;
      task.Task.kind <- kind
  | Miou.Trace.Spawn_location { uid; filename; line } ->
      let task = Tree.task g.tree uid in
      task.Task.location <- Some (filename, line)
  | Miou.Trace.Run_begin uid ->
      let task = Tree.task g.tree uid in
      task.Task.state <- State.Running;
      task.Task.start_at <- ts;
      let domain = Domains.get g.G.domains ring_id in
      domain.active_task <- Some uid;
      if
        (not domain.Domain.is_active)
        && Int64.compare domain.Domain.last_ts 0L > 0
      then begin
        let delta = Int64.sub ts domain.Domain.last_ts in
        Domain.add domain ~ts ~active:delta ~idle:0L
      end;
      domain.Domain.is_active <- true;
      domain.Domain.last_ts <- ts
  | Miou.Trace.Run_end uid ->
      let task = Tree.task g.tree uid in
      if Int64.compare task.Task.start_at 0L > 0 then begin
        let dur = Int64.to_int (Int64.sub ts task.Task.start_at) in
        if dur > 0 then
          ignore (Hdr_histogram.record_value task.Task.histogram dur);
        task.Task.count <- task.Task.count + 1
      end;
      task.Task.state <- State.Idle;
      let domain = Domains.get g.domains ring_id in
      domain.Domain.active_task <- None;
      if domain.Domain.is_active then begin
        let delta = Int64.sub ts domain.Domain.last_ts in
        Domain.add domain ~ts ~active:delta ~idle:0L
      end;
      domain.Domain.is_active <- false;
      domain.Domain.last_ts <- ts
  | Miou.Trace.Await uid ->
      let task = Tree.task g.G.tree uid in
      task.Task.state <- Awaiting
  | Miou.Trace.Cancelled uid ->
      let task = Tree.task g.G.tree uid in
      task.Task.state <- Cancelled
  | Miou.Trace.Run_done uid ->
      let task = Tree.task g.G.tree uid in
      task.Task.state <- Finished
  | _ -> ()

let run pid runtime_dir =
  Miou_unix.run ~domains:0 @@ fun () ->
  let cursor = Runtime_events.create_cursor (Some (runtime_dir, pid)) in
  let r = G.r () and g = G.create () in
  let lost_events _ring_id count = g.lost <- g.lost + count in
  let cbs = Runtime_events.Callbacks.create ~lost_events () in
  let cbs = Miou_runtime_events.add_callbacks ~fn:(fn g) cbs in
  let prm =
    Miou.async @@ fun () ->
    let rec go tick =
      let _n = Runtime_events.read_poll cursor cbs None in
      if tick mod 10 = 0 then Domains.tick g.G.domains;
      G.notify r;
      Miou_unix.sleep 0.01;
      go (tick + 1)
    in
    go 0
  in
  Nottui_miou.run (View.root r g);
  Miou.cancel prm;
  Runtime_events.free_cursor cursor

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

let term =
  let open Term in
  const run $ pid $ runtime_dir

let cmd =
  let doc = "Live TUI monitor for Miou applications." in
  let info = Cmd.info "mtop" ~doc in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
