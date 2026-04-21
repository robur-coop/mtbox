open Cmdliner

let output_options = "OUTPUT OPTIONS"

let verbosity =
  let env = Cmd.Env.info "MTBOX_LOGS" in
  Logs_cli.level ~docs:output_options ~env ()

let renderer =
  let env = Cmd.Env.info "MTBOX_FMT" in
  Fmt_cli.style_renderer ~docs:output_options ~env ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  let env = Cmd.Env.info "MTBOX_UTF_8" in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let setup_logs utf_8 style_renderer level =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stderr);
  Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)
let inhibit fn = try fn () with _ -> ()

let split_argv argv =
  let rec go acc = function
    | [] -> (List.rev acc, [])
    | "--" :: cmd -> (List.rev cmd, acc)
    | x :: r -> go (x :: acc) r
  in
  match go [] (List.rev (Array.to_list argv)) with
  | argv', exec -> (Array.of_list argv', exec)

let spawn ?(stdin = Unix.stdin) ?(stdout = Unix.stdout) ?(stderr = Unix.stderr)
    ~runtime_dir = function
  | [] -> invalid_arg "Mtbox.spawn: empty argv"
  | program :: _ as argv ->
      let argv = Array.of_list argv in
      let env =
        Array.append
          [|
             "OCAML_RUNTIME_EVENTS_START=1"; "MIOU_TRACE=1"
           ; Fmt.str "OCAML_RUNTIME_EVENTS_DIR=%s" runtime_dir
          |]
          (Unix.environment ())
      in
      let pid = Unix.create_process_env program argv env stdin stdout stderr in
      let filepath = Filename.concat runtime_dir (Fmt.str "%d.events" pid) in
      let deadline = Unix.gettimeofday () +. 5. in
      while
        (not (Sys.file_exists filepath)) && Unix.gettimeofday () < deadline
      do
        Unix.sleepf 0.02
      done;
      if not (Sys.file_exists filepath) then begin
        inhibit (fun () -> Unix.kill pid Sys.sigkill);
        inhibit (fun () -> ignore (Unix.waitpid [] pid));
        Fmt.failwith
          "Program %d never initialized Runtime_events; ensure the program \
           calls [Runtime_events.start ()] and [Miou.Trace.set_reporter \
           Miou_runtime_events.reporter]"
          pid
      end;
      pid
