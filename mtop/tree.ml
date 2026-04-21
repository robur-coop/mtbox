type t = (int, Task.t) Hashtbl.t

let task t uid =
  match Hashtbl.find_opt t uid with
  | Some task -> task
  | None ->
      let task = Task.create uid in
      Hashtbl.replace t uid task; task

let find t uid = Hashtbl.find_opt t uid

let remove t uid =
  match Hashtbl.find_opt t uid with
  | None -> ()
  | Some task ->
      Hdr_histogram.close task.Task.poll_histogram;
      Hdr_histogram.close task.Task.scheduled_histogram;
      Hashtbl.remove t uid

(* Upper bound on how long a task may stay [Idle] before we sweep it, in the
   absence of an explicit [Run_done]/[Cancelled]. [Run_done] can be lost when
   the runtime events ring overflows (e.g. a chatty TCP peer floods the ring),
   and without this escape hatch those tasks would accumulate as ghosts. *)
let stale_idle_s = 5.0

let sweep ?(keep = fun _ -> false) ?(paused = false) t ~now ~max_age_s =
  let stale = ref [] in
  let fn uid (task : Task.t) =
    let reclaimable =
      match task.state with
      | State.Finished | State.Cancelled -> true
      | _ -> false
    in
    (* [Idle]-ghost detection compares [now] to [run_mark]. When paused no
       events are applied, so [run_mark] stops advancing and every task would
       look stale - skip the check while paused. *)
    let idle_ghost =
      (not paused)
      && task.state = State.Idle
      && task.run_mark > 0.0
      && now -. task.run_mark > stale_idle_s
    in
    (* While paused, strip every terminal task except the one under the cursor
       immediately - otherwise [Finished] tasks pile up with no upper bound
       since live activity keeps minting new ones. *)
    let terminal =
      if paused then reclaimable
      else
        reclaimable
        && task.finish_mark > 0.0
        && now -. task.finish_mark > max_age_s
    in
    if (terminal || idle_ghost) && not (keep uid) then stale := uid :: !stale
  in
  Hashtbl.iter fn t;
  List.iter (remove t) !stale;
  List.length !stale

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

let decorated_line ~now ~selected ?(is_root = false) ?(prefix = "")
    ?(is_last = false) task =
  let line =
    if is_root then Task.label ~now task
    else Task.render ~now ~is_last ~prefix task
  in
  match selected with
  | Some uid when uid = task.Task.uid ->
      let w = (Ui.layout_spec line).Ui.w in
      let w = Int.max w 120 in
      Ui.resize ~w ~sw:1 ~bg:A.(bg (gray 14)) line
  | _ -> line

let rec render_task ~now ~selected ~compare t ?(prefix = "") ?(is_last = false)
    task =
  let line = decorated_line ~now ~selected ~prefix ~is_last task in
  let children = children t task.Task.uid in
  let children = List.sort compare children in
  let prefix = prefix ^ if is_last then tree_pad else tree_pipe in
  let lines = render_children ~now ~selected ~compare t ~prefix children in
  line :: lines

and render_children ~now ~selected ~compare t ?(prefix = "") = function
  | [] -> []
  | children ->
      let len = List.length children in
      let idx = ref 0 in
      let fn child =
        let is_last = !idx = len - 1 in
        let lines =
          render_task ~now ~selected ~compare t ~prefix ~is_last child
        in
        incr idx; lines
      in
      List.concat_map fn children

let rec collect_order_task ~compare t ?(uids = []) task =
  let uids = task.Task.uid :: uids in
  let children = children t task.Task.uid in
  let children = List.sort compare children in
  List.fold_left
    (fun acc c -> collect_order_task ~compare t ~uids:acc c)
    uids children

let visible_order ?(compare = Task.compare) t =
  let roots = List.sort compare (roots t) in
  let uids =
    List.fold_left
      (fun acc r -> collect_order_task ~compare t ~uids:acc r)
      [] roots
  in
  List.rev uids

let render ?(compare = Task.compare) tasks t ~selected =
  let open Lwd.Infix in
  Lwd.get tasks >>= fun () ->
  selected >|= fun selected ->
  let now = Unix.gettimeofday () in
  let roots = roots t in
  match List.sort compare roots with
  | [] -> Nottui_widgets.string ~attr:A.(fg (gray 12)) "No tasks yet..."
  | roots ->
      let hdr =
        Nottui_widgets.string ~attr:A.(fg white ++ A.st bold) "Task Tree"
      in
      let lines =
        let fn task =
          let root = decorated_line ~now ~selected ~is_root:true task in
          let children = children t task.Task.uid in
          let children = List.sort compare children in
          let children =
            render_children ~now ~selected ~compare t ~prefix:"" children
          in
          root :: children
        in
        List.concat_map fn roots
      in
      Ui.vcat (hdr :: lines)
