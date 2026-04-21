open Notty
open Nottui

let header (task : Task.t) =
  let st = task.state in
  let glyph = State.glyph st in
  let loc =
    match task.location with
    | None -> "<unknown>"
    | Some (f, l) -> Fmt.str "%s:%d" f l
  in
  let kind = Task.string_of_kind task.kind in
  Nottui_widgets.fmt
    ~attr:A.(fg white ++ st bold)
    "Task #%d  %s %s  dom%d  %s  %s" task.uid glyph (State.to_string st)
    task.runner kind loc

let kvf ?(attr = A.(fg white)) label fmt =
  let fn value =
    Ui.hcat
      [
        Nottui_widgets.fmt ~attr:A.(fg (gray 12)) "%-18s" label
      ; Nottui_widgets.string ~attr value
      ]
  in
  Fmt.kstr fn fmt

let pp_ns = Fmt.using Int64.to_int Stats.pp_ns

let counters_grid (tree : Tree.t) (task : Task.t) =
  let n_children = List.length (Tree.children tree task.uid) in
  let n_resources = List.length task.resources in
  let col1 =
    Ui.vcat
      [
        kvf "polls" "%d" task.count; kvf "yields" "%d" task.yields
      ; kvf "wakes" "%d" task.wakes; kvf "suspends" "%d" task.suspends
      ]
  in
  let col2 =
    Ui.vcat
      [
        kvf "children" "%d" n_children; kvf "resources" "%d" n_resources
      ; kvf "total busy" "%a" pp_ns task.total_busy_ns
      ; kvf "total scheduled" "%a" pp_ns task.total_scheduled_ns
      ; kvf "max blocking" "%a" pp_ns task.max_blocking_ns
      ]
  in
  Ui.hcat [ col1; Nottui_widgets.string "   "; col2 ]

let rec parent_chain tree uid acc =
  match Tree.find tree uid with
  | None -> List.rev acc
  | Some task ->
      let acc = task :: acc in
      if task.Task.parent < 0 || task.Task.parent = task.Task.uid then
        List.rev acc
      else parent_chain tree task.Task.parent acc

let breadcrumb tree (task : Task.t) =
  if task.parent < 0 then Nottui_widgets.string ~attr:A.(fg (gray 12)) "<root>"
  else
    let chain = parent_chain tree task.parent [] in
    let parts = List.map (fun (t : Task.t) -> Fmt.str "#%d" t.uid) chain in
    let parts = parts @ [ Fmt.str "#%d" task.uid ] in
    Nottui_widgets.string
      ~attr:A.(fg white)
      (String.concat " \xe2\x86\x92 " parts)

let children_list tree (task : Task.t) =
  let children = Tree.children tree task.uid in
  let children = List.sort Task.compare children in
  match children with
  | [] -> Nottui_widgets.string ~attr:A.(fg (gray 12)) "  (none)"
  | children ->
      let now = Unix.gettimeofday () in
      let fn (c : Task.t) =
        let st = Task.effective_state ~now c in
        Nottui_widgets.fmt ~attr:(State.attr st) "  %s #%-10d %-10s"
          (State.glyph st) c.uid (State.to_string st)
      in
      Ui.vcat (List.map fn children)

let resources_list (task : Task.t) =
  match task.resources with
  | [] -> Nottui_widgets.string ~attr:A.(fg (gray 12)) "  (none)"
  | rs ->
      let attr = A.(fg white) in
      let s =
        String.concat ", "
          (List.map (fun r -> Fmt.str "#%d" r) (List.sort Int.compare rs))
      in
      Nottui_widgets.string ~attr ("  " ^ s)

let histogram_block ~label ~count h =
  if count = 0 then
    Ui.vcat
      [
        Nottui_widgets.fmt ~attr:A.(fg white ++ st bold) "%s" label
      ; Nottui_widgets.string ~attr:A.(fg (gray 12)) "  (no data yet)"
      ]
  else
    let p50 = Hdr_histogram.value_at_percentile h 50. in
    let p90 = Hdr_histogram.value_at_percentile h 90. in
    let p99 = Hdr_histogram.value_at_percentile h 99. in
    let bar = Stats.histogram_bar ~width:60 h in
    let header =
      Nottui_widgets.fmt ~attr:A.(fg white ++ st bold) "%s  (n=%d)" label count
    in
    let percentiles =
      Ui.hcat
        [
          Nottui_widgets.fmt ~attr:A.(fg cyan) "  p50=%a" Stats.pp_ns p50
        ; Nottui_widgets.fmt ~attr:A.(fg yellow) "   p90=%a" Stats.pp_ns p90
        ; Nottui_widgets.fmt ~attr:A.(fg red) "   p99=%a" Stats.pp_ns p99
        ]
    in
    let scale =
      Nottui_widgets.string
        ~attr:A.(fg (gray 10))
        "  100ns      1us        10us       100us      1ms        10ms       \
         100ms"
    in
    Ui.vcat
      [
        header; Ui.hcat [ Nottui_widgets.string "  "; bar ]; scale; percentiles
      ]

let section name body =
  let hdr = Nottui_widgets.fmt ~attr:A.(fg white ++ st bold) "%s" name in
  Ui.vcat [ hdr; body ]

let blocked_on (task : Task.t) =
  match task.state with
  | State.Suspended name ->
      Ui.hcat
        [
          Nottui_widgets.fmt ~attr:A.(fg (gray 12)) "%-18s" "blocked on"
        ; Nottui_widgets.string ~attr:A.(fg yellow ++ st bold) name
        ]
  | _ ->
      Ui.hcat
        [
          Nottui_widgets.fmt ~attr:A.(fg (gray 12)) "%-18s" "blocked on"
        ; Nottui_widgets.string ~attr:A.(fg (gray 12)) "-"
        ]

let render (g : G.t) uid =
  match Tree.find g.tree uid with
  | None ->
      Nottui_widgets.fmt
        ~attr:A.(fg red)
        "Task #%d no longer exists. Press Backspace." uid
  | Some task ->
      let hint =
        Nottui_widgets.string
          ~attr:A.(fg (gray 12))
          "Backspace: back  |  j/k: scroll"
      in
      Ui.vcat
        [
          header task; hint; Nottui_widgets.string ""; blocked_on task
        ; Nottui_widgets.string ""
        ; section "Counters" (counters_grid g.tree task)
        ; Nottui_widgets.string ""
        ; section "Parent chain" (breadcrumb g.tree task)
        ; Nottui_widgets.string ""
        ; section "Children" (children_list g.tree task)
        ; Nottui_widgets.string ""; section "Resources" (resources_list task)
        ; Nottui_widgets.string ""
        ; histogram_block ~label:"Poll Times Percentiles" ~count:task.count
            task.poll_histogram; Nottui_widgets.string ""
        ; histogram_block ~label:"Scheduled Times Percentiles"
            ~count:task.scheduled_count task.scheduled_histogram
        ]
