type sample = { ts: int64; active: int64; idle: int64 }

type t = {
    uid: int
  ; mutable active_task: int option
  ; mutable last_ts: int64
  ; mutable is_active: bool
  ; samples: sample Queue.t
  ; sparkline: float array
  ; mutable spark: int
}

let create uid =
  let active_task = None
  and last_ts = 0L
  and is_active = false
  and samples = Queue.create ()
  and sparkline = Array.make 60 0.0
  and spark = 0 in
  { uid; active_task; last_ts; is_active; samples; sparkline; spark }

let compare { uid= uid0; _ } { uid= uid1; _ } = Int.compare uid0 uid1
let window = 2_000_000_000L (* 2s *)

let add t ~ts ~active ~idle =
  let elt = { ts; active; idle } in
  Queue.push elt t.samples;
  let cutoff = Int64.sub ts window in
  while
    (not (Queue.is_empty t.samples))
    && Int64.compare (Queue.peek t.samples).ts cutoff < 0
  do
    ignore (Queue.pop t.samples)
  done

let pct t =
  let active = ref 0L and idle = ref 0L in
  let fn s =
    active := Int64.add !active s.active;
    idle := Int64.add !idle s.idle
  in
  Queue.iter fn t.samples;
  let total = Int64.add !active !idle in
  if Int64.compare total 0L > 0 then
    Int64.to_float !active /. Int64.to_float total
  else 0.0

let tick = 100_000_000L (* ~100ms, matches polling interval. *)

let tick t =
  if (not t.is_active) && Int64.compare t.last_ts 0L > 0 then begin
    let ts = Int64.add t.last_ts tick in
    add t ~ts ~active:0L ~idle:tick;
    t.last_ts <- ts
  end;
  t.sparkline.(t.spark mod 60) <- pct t;
  t.spark <- t.spark + 1

let sparkline t =
  let n = Int.min t.spark 60 in
  let result = Array.make n 0.0 in
  let start = if t.spark > 60 then t.spark mod 60 else 0 in
  for idx = 0 to n - 1 do
    result.(idx) <- t.sparkline.((start + idx) mod 60)
  done;
  result

let ld = [| 0x00; 0x40; 0x44; 0x46; 0x47 |]
let rd = [| 0x00; 0x80; 0xa0; 0xb0; 0xb8 |]

let graph data =
  let n = Array.length data in
  if n = 0 then String.make (60 / 2) ' '
  else
    let buf = Buffer.create (n * 3) in
    let quantize v = Int.max 0 (Int.min 4 (Float.to_int ((v *. 4.0) +. 0.5))) in
    let pairs = (n + 1) / 2 in
    for idx = 0 to pairs - 1 do
      let l = quantize data.(idx * 2) in
      let r = if (idx * 2) + 1 < n then quantize data.((idx * 2) + 1) else 0 in
      let code = 0x2800 + ld.(l) + rd.(r) in
      Buffer.add_utf_8_uchar buf (Uchar.of_int code)
    done;
    let pad = (60 / 2) - pairs in
    for _ = 1 to pad do
      Buffer.add_utf_8_uchar buf (Uchar.of_int 0x2800)
    done;
    Buffer.contents buf

open Notty
open Nottui

let render t =
  let pct = pct t in
  let sparkline = sparkline t in
  let sparkline = graph sparkline in
  let task =
    match t.active_task with Some uid -> Fmt.str "#%d" uid | None -> "idle"
  in
  let label = Fmt.str "dom%-2d %5.1f%%" t.uid (pct *. 100.) in
  let suffix = Fmt.str " %s" task in
  let spark =
    if pct > 0.8 then A.(fg red)
    else if pct > 0.5 then A.(fg yellow)
    else A.(fg green)
  in
  Ui.hcat
    [
      Nottui_widgets.string ~attr:A.(fg white) label
    ; Nottui_widgets.string ~attr:spark sparkline
    ; Nottui_widgets.string ~attr:A.(fg (gray 16)) suffix
    ]
