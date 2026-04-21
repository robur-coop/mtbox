let default_histogram_width = 30
let log_min = log 100.0 (* 100ns *)
let log_max = log 1e8 (* 100ms *)
let log_span = log_max -. log_min

let bin_of_ns ~width v =
  let n_bins = width * 2 in
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
  if n < 1_000 then Fmt.pf ppf "%d" n
  else if n < 10_000 then Fmt.pf ppf "%.1fk" (float_of_int n /. 1e3)
  else if n < 1_000_000 then Fmt.pf ppf "%dk" (n / 1_000)
  else if n < 10_000_000 then Fmt.pf ppf "%.1fM" (float_of_int n /. 1e6)
  else if n < 1_000_000_000 then Fmt.pf ppf "%dM" (n / 1_000_000)
  else Fmt.pf ppf "%.1fG" (float_of_int n /. 1e9)

let ld = [| 0x00; 0x40; 0x44; 0x46; 0x47 |]
let rd = [| 0x00; 0x80; 0xa0; 0xb0; 0xb8 |]

let segment heights a b =
  let buf = Buffer.create ((b - a + 1) * 3) in
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

let decades = 6 (* 100ns, 1us, 10us, 100us, 1ms, 10ms, 100ms *)

let histogram_bar ?(width = default_histogram_width) h =
  let n_bins = width * 2 in
  let bins = Array.make n_bins 0 in
  for p = 1 to 99 do
    let v = Hdr_histogram.value_at_percentile h (float_of_int p) in
    let idx = bin_of_ns ~width v in
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
  let p50_char = bin_of_ns ~width p50_v / 2 in
  let p90_char = bin_of_ns ~width p90_v / 2 in
  let color_of c =
    if c <= p50_char then A.green else if c <= p90_char then A.yellow else A.red
  in
  let chars_per_decade = width / decades in
  let sep = Nottui_widgets.string ~attr:A.(fg (gray 8)) "\xe2\x94\x8a" in
  let parts = ref [] in
  let flush_segment start_c end_c =
    if start_c <= end_c then begin
      let str = segment heights start_c end_c in
      let attr = A.fg (color_of start_c) in
      parts := Nottui_widgets.string ~attr str :: !parts
    end
  in
  let seg_start = ref 0 in
  let current_color = ref (color_of 0) in
  for c = 0 to width - 1 do
    if c > !seg_start && color_of c <> !current_color then begin
      flush_segment !seg_start (c - 1);
      seg_start := c;
      current_color := color_of c
    end;
    if c > 0 && c mod chars_per_decade = 0 then begin
      flush_segment !seg_start (c - 1);
      parts := sep :: !parts;
      seg_start := c;
      current_color := color_of c
    end
  done;
  flush_segment !seg_start (width - 1);
  parts := sep :: !parts;
  Ui.hcat (List.rev !parts)
