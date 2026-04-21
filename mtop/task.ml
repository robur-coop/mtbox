(* Per-task state reconstructed from Miou's runtime_events stream.

   Two time bases coexist:
   - *_at / *_start / *_ts : int64 nanoseconds from the runtime_events clock.
     Monotonic and ring-local, suitable for ns-precision deltas between
     ordered events from the same ring (e.g. [Run_begin] -> [Run_end]).
   - *_mark : float wall time ([Unix.gettimeofday]). Used for UI-facing
     heuristics that compare against "now" (sticky-state windows, sweep
     grace). We cannot convert the runtime events clock to wall time without
     an anchor, hence the duplication. *)
type t = {
    uid: int
  ; mutable parent: int
  ; mutable runner: int
  ; mutable kind: [ `Async | `Parallel ]
  ; mutable location: (string * int) option
  ; mutable state: State.t
  ; mutable run_mark: float (* Last [Run_begin] (wall). For sticky-[Running]. *)
  ; mutable finish_mark: float
        (* [Run_done] or [Cancelled] (wall). Sweep grace reference. *)
  ; mutable wake_mark: float
        (* Last [Resume] -> [Waking] transition (wall). Drives sticky-[Waking]
         so the state is visible at UI refresh rate even though it lasts only
         microseconds before [Run_begin]. *)
  ; mutable yield_mark: float
        (* Last [Yield] (wall). Drives sticky-[Yielded], same reasoning as
           above. *)
  ; mutable start_at: int64
        (* Current [Run_begin] ts. [poll_histogram] / [total_busy_ns] feeder. *)
  ; mutable ready_at: int64
        (* [Ready]-to-run ts ([Spawn], [Resume], [Yield]). Feeds [scheduled_histogram]
         against the next [Run_begin]. *)
  ; mutable suspend_start: int64
        (* Current [Suspend] ts. Diffed against [Continue] for
         [max_blocking_ns]. *)
  ; mutable last_ts: int64
        (* Largest ts seen from a ring-local event. Used to discard stale
         [Resume] events whose ts predates our last ring-local event. *)
  ; poll_histogram: Hdr_histogram.t
  ; scheduled_histogram: Hdr_histogram.t
  ; mutable count: int
  ; mutable scheduled_count: int
  ; mutable yields: int
  ; mutable wakes: int
  ; mutable suspends: int
  ; mutable total_busy_ns: int64
  ; mutable total_scheduled_ns: int64
  ; mutable max_blocking_ns: int64
  ; mutable resources: int list
}

let histogram () =
  Hdr_histogram.init ~lowest_discernible_value:1
    ~highest_trackable_value:1_000_000_000 ~significant_figures:3

let create uid =
  {
    uid
  ; parent= -1
  ; runner= 0
  ; kind= `Async
  ; location= None
  ; state= State.Idle
  ; start_at= 0L
  ; ready_at= 0L
  ; last_ts= 0L
  ; run_mark= 0.0
  ; finish_mark= 0.0
  ; wake_mark= 0.0
  ; yield_mark= 0.0
  ; poll_histogram= histogram ()
  ; scheduled_histogram= histogram ()
  ; count= 0
  ; scheduled_count= 0
  ; yields= 0
  ; wakes= 0
  ; suspends= 0
  ; suspend_start= 0L
  ; max_blocking_ns= 0L
  ; total_busy_ns= 0L
  ; total_scheduled_ns= 0L
  ; resources= []
  }

let running_sticky_s = 0.15
let transient_sticky_s = 0.1

(* Display-layer state smoothing:
   - sticky [Running]: [Idle]/[Finished] tasks with a fresh [Run_begin] keep
     showing [Running]. Covers [Run_end] -> [Run_begin] flicker and whole
     lifetimes that fit in one UI tick.
   - sticky [Waking] / [Yielded]: after the transient state collapses into
     [Running] ([Run_begin] fires within microseconds), keep showing the prior
     transition briefly so it's actually visible at 10Hz refresh.

   The most recent mark wins if both are fresh. *)
let effective_state ~now t =
  match t.state with
  | State.Running
    when now -. t.wake_mark < transient_sticky_s
         && t.wake_mark > 0.0
         && t.wake_mark >= t.yield_mark ->
      State.Waking
  | State.Running
    when now -. t.yield_mark < transient_sticky_s && t.yield_mark > 0.0 ->
      State.Yielded
  | (State.Idle | State.Finished)
    when now -. t.run_mark < running_sticky_s && t.run_mark > 0.0 ->
      State.Running
  | _ -> t.state

let string_of_kind = function `Async -> "async" | `Parallel -> "parallel"
let compare { uid= uid0; _ } { uid= uid1; _ } = Int.compare uid0 uid1

open Notty
open Nottui

let label ~now t =
  let st = effective_state ~now t in
  let state = State.attr st in
  let kind =
    match t.kind with `Async -> A.(fg blue) | `Parallel -> A.(fg magenta)
  in
  let loc =
    match t.location with
    | None -> ""
    | Some (filename, line) -> Fmt.str "%s:%d" filename line
  in
  let glyph = State.glyph st in
  Ui.hcat
    [
      Nottui_widgets.fmt ~attr:A.(fg white) "[%-10d] " t.uid
    ; Nottui_widgets.fmt ~attr:A.(fg (gray 16)) "dom%-2d " t.runner
    ; Nottui_widgets.fmt ~attr:kind "%-9s" (string_of_kind t.kind)
    ; Nottui_widgets.fmt ~attr:state "%s %-14s" glyph (State.to_string st)
    ; Nottui_widgets.string ~attr:A.(fg (gray 12)) loc
    ]

let tree_tee = "\xe2\x94\x9c\xe2\x94\x80" (* "├─" *)
let tree_end = "\xe2\x94\x94\xe2\x94\x80" (* "└─" *)

let render ~now ?(is_last = false) ?(prefix = "") t =
  let branch = if is_last then tree_end else tree_tee in
  Ui.hcat
    [
      Nottui_widgets.fmt ~attr:A.(fg (gray 8)) "%s%s" prefix branch
    ; label ~now t
    ]
