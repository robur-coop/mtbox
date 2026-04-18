type t = {
    uid: int
  ; mutable parent: int
  ; mutable runner: int
  ; mutable kind: [ `Async | `Parallel ]
  ; mutable location: (string * int) option
  ; mutable state: State.t
  ; mutable start_at: int64
  ; histogram: Hdr_histogram.t
  ; mutable count: int
}

let histogram () =
  Hdr_histogram.init ~lowest_discernible_value:1
    ~highest_trackable_value:1_000_000_000 ~significant_figures:3

let create ?(parent = -1) ?(runner = 0) ?(kind = `Async) ?location
    ?(state = State.Idle) ?(start_at = 0L) ?(histogram = histogram ())
    ?(count = 0) uid =
  { uid; parent; runner; kind; location; state; start_at; histogram; count }

let string_of_kind = function `Async -> "async" | `Parallel -> "parallel"
let compare { uid= uid0; _ } { uid= uid1; _ } = Int.compare uid0 uid1
let is_alive { state; _ } = state <> State.Finished

open Notty
open Nottui

let label t =
  let state = State.attr t.state in
  let kind =
    match t.kind with `Async -> A.(fg blue) | `Parallel -> A.(fg magenta)
  in
  let loc =
    match t.location with
    | None -> ""
    | Some (filename, line) -> Fmt.str "%s:%d" filename line
  in
  Ui.hcat
    [
      Nottui_widgets.fmt ~attr:A.(fg white) "[%-3d] " t.uid
    ; Nottui_widgets.fmt ~attr:A.(fg (gray 16)) "dom%-2d " t.runner
    ; Nottui_widgets.fmt ~attr:kind "%-9s" (string_of_kind t.kind)
    ; Nottui_widgets.fmt ~attr:state "%-16s" (State.to_string t.state)
    ; Nottui_widgets.string ~attr:A.(fg (gray 12)) loc
    ]

let tree_tee = "\xe2\x94\x9c\xe2\x94\x80" (* "├─" *)
let tree_end = "\xe2\x94\x94\xe2\x94\x80" (* "└─" *)

let render ?(is_last = false) ?(prefix = "") t =
  let branch = if is_last then tree_end else tree_tee in
  Ui.hcat
    [ Nottui_widgets.fmt ~attr:A.(fg (gray 8)) "%s%s" prefix branch; label t ]
