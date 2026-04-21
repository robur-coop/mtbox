let inhibit fn = try fn () with _exn -> ()

let cpu_bound =
  let rec busy n acc =
    if n = 0 then acc else busy (n - 1) (acc * 2654435761 lxor n)
  in
  let rec go () =
    let _ = busy 200_000 0 in
    Miou_unix.sleep 0.0001; go ()
  in
  go

let periodic_sleeper interval =
  let rec go i =
    Miou_unix.sleep interval;
    go (i + 1)
  in
  go 0

let occasional_long_block =
  let rec go () = Miou_unix.sleep 0.02; Miou_unix.sleep 0.12; go () in
  go

let echo fd =
  let buf = Bytes.create 65536 in
  let finally = Miou_unix.close in
  let res = Miou.Ownership.create ~finally fd in
  Miou.Ownership.own res;
  let rec go () =
    let n = Miou_unix.read fd buf in
    if n > 0 then begin
      let s = Bytes.sub_string buf 0 n in
      Miou_unix.write fd s; go ()
    end
  in
  go (); Miou.Ownership.release res

let tcp_server port =
  let sock = Miou_unix.tcpv4 () in
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Miou_unix.bind_and_listen sock addr;
  let orphans = Miou.orphans () in
  let rec clean () =
    match Miou.care orphans with
    | Some (Some p) ->
        inhibit (fun () -> Miou.await_exn p);
        clean ()
    | _ -> ()
  in
  let rec go () =
    let fd, _ = Miou_unix.accept sock in
    let _ = Miou.call ~orphans (fun () -> echo fd) in
    clean (); go ()
  in
  go ()

let parent_that_gets_cancelled () =
  let rec forever () = Miou_unix.sleep 5.0; forever () in
  let child = Miou.async forever in
  let _ : unit = Miou_unix.sleep 0.5 |> Fun.id in
  Miou_unix.sleep 60.0; ignore child

let cancellation_harness =
  let rec go () =
    let parent = Miou.async parent_that_gets_cancelled in
    Miou_unix.sleep 2.0; Miou.cancel parent; Miou_unix.sleep 3.0; go ()
  in
  go

let fan_out_parallel =
  let rec go () =
    let _ =
      Miou.parallel
        (fun n ->
          let rec busy i acc =
            if i = 0 then acc else busy (i - 1) (acc * n lxor i)
          in
          ignore (busy 50_000 1))
        [ 1; 2; 3; 4; 5; 6; 7; 8 ]
    in
    Miou_unix.sleep 0.3; go ()
  in
  go

let yielder =
  let rec go () = Miou.yield (); Miou_unix.sleep 0.02; go () in
  go

let churn =
  let fn idx =
    Miou.async @@ fun () -> Miou_unix.sleep (0.05 +. (float_of_int idx *. 0.01))
  in
  let rec go () =
    let workers = List.init 4 fn in
    List.iter Miou.await_exn workers;
    Miou_unix.sleep 0.1;
    go ()
  in
  go

let wait_for_enter () =
  Fmt.pr "Press <Enter> to start the workload...%!";
  inhibit (fun () -> ignore (input_line stdin))

let () =
  if Domain.recommended_domain_count () < 4 then
    failwith "This program requires, at least, 4 CPUs";
  let domains =
    try int_of_string (Sys.getenv "DEMO_DOMAINS")
    with _ -> Int.min 3 (Domain.recommended_domain_count () - 1)
  in
  let port = try int_of_string (Sys.getenv "DEMO_PORT") with _ -> 9000 in
  Fmt.pr "PID %d, domains=%d, port=%d\n%!" (Unix.getpid ()) domains port;
  Miou.Trace.set_reporter Miou_runtime_events.reporter;
  wait_for_enter ();
  Miou_unix.run ~domains @@ fun () ->
  let tasks =
    [
      Miou.call cpu_bound; Miou.async yielder
    ; Miou.async (fun () -> periodic_sleeper 0.3)
    ; Miou.async (fun () -> periodic_sleeper 0.05)
    ; Miou.async occasional_long_block; Miou.async (fun () -> tcp_server port)
    ; Miou.async cancellation_harness; Miou.async fan_out_parallel
    ; Miou.async churn
    ]
  in
  List.iter Miou.await_exn tasks
