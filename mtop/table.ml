open Notty
open Nottui

let rank task = State.rank task.Task.state

let comparator (mode : G.sort) desc =
  let base a b =
    match mode with
    | G.Uid -> Int.compare a.Task.uid b.Task.uid
    | Busy -> Int64.compare a.Task.total_busy_ns b.Task.total_busy_ns
    | Polls -> Int.compare a.Task.count b.Task.count
    | Wakes -> Int.compare a.Task.wakes b.Task.wakes
    | State -> Int.compare (rank a) (rank b)
  in
  if desc then fun a b -> -base a b else base

let header =
  let attr = A.(fg white ++ st bold ++ st underline) in
  Nottui_widgets.fmt ~attr " %-10s %-6s %-8s %-11s %-6s %-8s %-8s %-5s %s" "ID"
    "DOM" "KIND" "STATE" "POLLS" "BUSY" "SCHED" "WAKES" "LOCATION"

let row ~now ~selected task =
  let st = Task.effective_state ~now task in
  let glyph = State.glyph st in
  let state =
    let s = State.to_string st in
    if String.length s > 9 then String.sub s 0 9 else s
  in
  let kind_attr =
    match task.Task.kind with
    | `Async -> A.(fg blue)
    | `Parallel -> A.(fg magenta)
  in
  let loc =
    match task.Task.location with
    | None -> "-"
    | Some (f, l) -> Fmt.str "%s:%d" f l
  in
  let busy = Fmt.str "%a" Stats.pp_ns (Int64.to_int task.Task.total_busy_ns) in
  let sched =
    Fmt.str "%a" Stats.pp_ns (Int64.to_int task.Task.total_scheduled_ns)
  in
  let base =
    Ui.hcat
      [
        Nottui_widgets.fmt ~attr:A.(fg white) " %-10d " task.uid
      ; Nottui_widgets.fmt ~attr:A.(fg (gray 16)) "%-6d " task.runner
      ; Nottui_widgets.fmt ~attr:kind_attr "%-8s "
          (Task.string_of_kind task.kind)
      ; Nottui_widgets.fmt ~attr:(State.attr st) "%s %-9s " glyph state
      ; Nottui_widgets.fmt
          ~attr:A.(fg white)
          "%-6s "
          (Fmt.str "%a" Stats.pp_count task.count)
      ; Nottui_widgets.fmt ~attr:A.(fg white) "%-8s " busy
      ; Nottui_widgets.fmt ~attr:A.(fg white) "%-8s " sched
      ; Nottui_widgets.fmt
          ~attr:A.(fg white)
          "%-5s "
          (Fmt.str "%a" Stats.pp_count task.wakes)
      ; Nottui_widgets.string ~attr:A.(fg (gray 14)) loc
      ]
  in
  match selected with
  | Some uid when uid = task.Task.uid ->
      let w = (Ui.layout_spec base).Ui.w in
      let w = Int.max w 120 in
      Ui.resize ~w ~sw:1 ~bg:A.(bg (gray 4)) base
  | _ -> base

let flat_order ~compare t =
  let tasks = Hashtbl.fold (fun _ task acc -> task :: acc) t [] in
  List.sort compare tasks

let render tasks t ~selected ~compare =
  let open Lwd.Infix in
  Lwd.get tasks >>= fun () ->
  selected >|= fun selected ->
  let now = Unix.gettimeofday () in
  let list = flat_order ~compare t in
  match list with
  | [] -> Nottui_widgets.string ~attr:A.(fg (gray 12)) "No tasks yet..."
  | list ->
      let rows = List.map (row ~now ~selected) list in
      Ui.vcat (header :: rows)

let visible_order ~compare t =
  let list = flat_order ~compare t in
  List.map (fun t -> t.Task.uid) list
