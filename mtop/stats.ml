let histogram_width = 30
let log_min = log 100.0 (* 100ns *)
let log_max = log 1e8 (* 100ms *)
let log_span = log_max -. log_min

let bin_of_ns v =
  let n_bins = histogram_width * 2 in
  let v = Float.max 100.0 (float_of_int (Int.max 1 v)) in
  let pos = (log v -. log_min) /. log_span *. float_of_int (n_bins - 1) in
  Int.max 0 (Int.min (n_bins - 1) (Float.to_int (pos +. 0.5)))

let pp_ns ppf ns =
  if ns < 1_000 then Fmt.pf ppf "%dns" ns
  else if ns < 1_000_000 then
    let us = float_of_int ns /. 1e3 in
    if us < 10.0 then Fmt.pf ppf "%.1fus" us else Fmt.pf ppf "%.0fus" us
  else if ns < 1_000_000_000 then
    let ms = float_of_int ns /. 1e6 in
    if ms < 10.0 then Fmt.pf ppf "%.1fms" ms else Fmt.pf ppf "%.0fms" ms
  else Fmt.pf ppf "%.1fs" (float_of_int ns /. 1e9)

let pp_count ppf n =
  if n < 1_000 then Fmt.pf ppf "%4d" n
  else if n < 1_000_000 then Fmt.pf ppf "%3dk" (n / 1_000)
  else if n < 1_000_000_000 then Fmt.pf ppf "%3dM" (n / 1_000_000)
  else Fmt.pf ppf "%3dG" (n / 1_000_000_000)

let ld = [| 0x00; 0x40; 0x44; 0x46; 0x47 |]
let rd = [| 0x00; 0x80; 0xa0; 0xb0; 0xb8 |]

let segment heights a b =
  let buf = Buffer.create ((a - b + 1) * 3) in
  for i = a to b do
    let li = i * 2 and ri = (i * 2) + 1 in
    let l = if li < Array.length heights then heights.(li) else 0 in
    let r = if ri < Array.length heights then heights.(ri) else 0 in
    let code = 0x2800 + ld.(l) + rd.(r) in
    Buffer.add_utf_8_uchar buf (Uchar.of_int code)
  done;
  Buffer.contents buf

open Notty
open Nottui

let box_h = "\xe2\x94\x80" (* ─ *)

let line len =
  let buf = Buffer.create (len * 3) in
  for _ = 1 to len do
    Buffer.add_string buf box_h
  done;
  Buffer.contents buf

let box_v = "\xe2\x94\x82" (* │ *)

let row task =
  let h = task.Task.histogram in
  let n_bins = histogram_width * 2 in
  let bins = Array.make n_bins 0 in
  for p = 1 to 99 do
    let v = Hdr_histogram.value_at_percentile h (float_of_int p) in
    let idx = bin_of_ns v in
    bins.(idx) <- bins.(idx) + 1
  done;
  let max_count = Array.fold_left Int.max 1 bins in
  let heights =
    let max_count = float_of_int max_count in
    let fn c =
      let c = float_of_int c in
      let v = (c /. max_count *. 4.) +. 0.5 in
      Int.max 0 (Int.min 4 (Float.to_int v))
    in
    Array.map fn bins
  in
  let p50_v = Hdr_histogram.value_at_percentile h 50. in
  let p90_v = Hdr_histogram.value_at_percentile h 90. in
  let p99_v = Hdr_histogram.value_at_percentile h 99. in
  let p50_char = bin_of_ns p50_v / 2 in
  let p90_char = bin_of_ns p90_v / 2 in
  let seg1_end = Int.min p50_char (histogram_width - 1) in
  let seg2_start = seg1_end + 1 in
  let seg2_end = Int.min p90_char (histogram_width - 1) in
  let seg3_start = seg2_end + 1 in
  let parts = ref [] in
  if seg1_end >= 0 then begin
    let str = segment heights 0 seg1_end in
    parts := Nottui_widgets.string ~attr:A.(fg green) str :: !parts
  end;
  if seg2_start <= seg2_end then begin
    let str = segment heights seg2_start seg2_end in
    parts := Nottui_widgets.string ~attr:A.(fg yellow) str :: !parts
  end;
  if seg3_start <= histogram_width - 1 then begin
    let str = segment heights seg2_start (histogram_width - 1) in
    parts := Nottui_widgets.string ~attr:A.(fg red) str :: !parts
  end;
  let ui = Ui.hcat (List.rev !parts) in
  let sep = Nottui_widgets.string ~attr:A.(fg (gray 8)) box_v in
  let open Nottui_widgets in
  Ui.hcat
    [
      sep; fmt ~attr:(State.attr task.Task.state) " #%-5d " task.Task.uid; sep
    ; fmt ~attr:A.(fg white) " %a " pp_count task.Task.count; sep
    ; fmt ~attr:A.empty " "; ui; fmt ~attr:A.empty " "; sep
    ; fmt ~attr:A.(fg cyan) " %-7s " (Fmt.to_to_string pp_ns p50_v); sep
    ; fmt ~attr:A.(fg magenta) " %-7s " (Fmt.to_to_string pp_ns p99_v); sep
    ]

let table_rule =
  Fmt.str
    "\xe2\x94\x9c%s\xe2\x94\xbc%s\xe2\x94\xbc%s\xe2\x94\xbc%s\xe2\x94\xbc%s\xe2\x94\xa4"
    (line 8) (line 6) (line 32) (line 9) (line 9)

let table_top =
  Fmt.str
    "\xe2\x94\x8c%s\xe2\x94\xac%s\xe2\x94\xac%s\xe2\x94\xac%s\xe2\x94\xac%s\xe2\x94\x90"
    (line 8) (line 6) (line 32) (line 9) (line 9)

let table_bottom =
  Fmt.str
    "\xe2\x94\x94%s\xe2\x94\xb4%s\xe2\x94\xb4%s\xe2\x94\xb4%s\xe2\x94\xb4%s\xe2\x94\x98"
    (line 8) (line 6) (line 32) (line 9) (line 9)

let table_title =
  let sep = Nottui_widgets.string ~attr:A.(fg (gray 8)) box_v in
  let hdr s w =
    Nottui_widgets.fmt ~attr:A.(fg white ++ A.st underline) " %-*s " (w - 2) s
  in
  Ui.hcat
    [
      sep; hdr "Task" 8; sep; hdr "Runs" 6; sep
    ; hdr ".1u  1u   10u  100u 1ms  10ms" 32; sep; hdr "p50" 9; sep; hdr "p99" 9
    ; sep
    ]

let render ~only_active tasks tree =
  let open Lwd.Infix in
  Lwd.get tasks >>= fun () ->
  Lwd.get only_active >|= fun only_active ->
  let tasks = Tree.list tree in
  let tasks =
    let fn task =
      task.Task.count > 0 && ((not only_active) || Task.is_alive task)
    in
    List.filter fn tasks
  in
  let tasks = List.sort Task.compare tasks in
  let open Nottui_widgets in
  let hdr = fmt ~attr:A.(fg white ++ st bold) "Task CPU Distribution" in
  match tasks with
  | [] ->
      let elt = fmt ~attr:A.(fg (gray 12)) "No execution data yet..." in
      Ui.join_y hdr elt
  | tasks ->
      let top = string ~attr:A.(fg (gray 8)) table_top in
      let rule = string ~attr:A.(fg (gray 8)) table_rule in
      let bottom = string ~attr:A.(fg (gray 8)) table_bottom in
      let rows = List.map row tasks in
      let interleaved = List.concat_map (fun row -> [ rule; row ]) rows in
      Ui.vcat ((hdr :: top :: table_title :: interleaved) @ [ bottom ])
