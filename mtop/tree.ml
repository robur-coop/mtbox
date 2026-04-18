type t = (int, Task.t) Hashtbl.t

let task t uid =
  match Hashtbl.find_opt t uid with
  | Some task -> task
  | None ->
      let task = Task.create uid in
      Hashtbl.replace t uid task; task

let list t =
  let fn _ task acc = task :: acc in
  Hashtbl.fold fn t []

let children t uid =
  let fn _ task acc = if task.Task.parent = uid then task :: acc else acc in
  Hashtbl.fold fn t []

let roots t =
  let fn _ task acc =
    if task.Task.uid < 0 || not (Hashtbl.mem t task.Task.parent) then
      task :: acc
    else acc
  in
  Hashtbl.fold fn t []

let tree_pad = "  "
let tree_pipe = "\xe2\x94\x82 " (* "│ " *)

open Notty
open Nottui

let rec render_task t ?(only_active = true) ?(prefix = "") ?(is_last = false)
    task =
  let line = Task.render ~is_last ~prefix task in
  let children = children t task.Task.uid in
  let children =
    let fn task = task.Task.state <> State.Finished in
    if only_active then List.filter fn children else children
  in
  let children = List.sort Task.compare children in
  let prefix = prefix ^ if is_last then tree_pad else tree_pipe in
  let lines = render_children t ~only_active ~prefix children in
  line :: lines

and render_children t ?(only_active = true) ?(prefix = "") = function
  | [] -> []
  | children ->
      let len = List.length children in
      let idx = ref 0 in
      let fn child =
        let is_last = !idx = len - 1 in
        let lines = render_task t ~only_active ~prefix ~is_last child in
        incr idx; lines
      in
      List.concat_map fn children

let render ~only_active tasks t =
  let open Lwd.Infix in
  Lwd.get tasks >>= fun () ->
  Lwd.get only_active >|= fun only_active ->
  let roots = roots t in
  let roots =
    let fn task = task.Task.state <> State.Finished in
    if only_active then List.filter fn roots else roots
  in
  match List.sort Task.compare roots with
  | [] -> Nottui_widgets.string ~attr:A.(fg (gray 12)) "No tasks yet..."
  | roots ->
      let hdr =
        Nottui_widgets.string ~attr:A.(fg white ++ A.st bold) "Task Tree"
      in
      let lines =
        let fn task =
          let root = Task.label task in
          let children = children t task.Task.uid in
          let children =
            let fn task = task.Task.state <> State.Finished in
            if only_active then List.filter fn children else children
          in
          let children = List.sort Task.compare children in
          let children = render_children t ~only_active ~prefix:"" children in
          root :: children
        in
        List.concat_map fn roots
      in
      Ui.vcat (hdr :: lines)
