open Notty
open Nottui

let mode = function
  | G.Tree -> "tree"
  | Table -> "table"
  | Detail _ -> "detail"
  | Resources -> "res"

let bar r g =
  let open Lwd.Infix in
  Lwd.get r.G.tasks >>= fun () ->
  Lwd.get r.G.ui_mode >>= fun ui ->
  Lwd.get r.G.sort_mode >>= fun sm ->
  Lwd.get r.G.sort_desc >|= fun sd ->
  let ntasks = Hashtbl.length g.G.tree in
  let ndomains = Hashtbl.length g.G.domains in
  let now = Unix.gettimeofday () in
  let running =
    let fn _ task acc =
      if Task.effective_state ~now task = State.Running then acc + 1 else acc
    in
    Hashtbl.fold fn g.G.tree 0
  in
  let sort =
    Fmt.str " | sort:%-5s%s" (G.sort_mode_to_string sm)
      (if sd then "\xe2\x86\x93" else "\xe2\x86\x91")
  in
  let mode = Fmt.str " | %-6s" (mode ui) in
  let base =
    Nottui_widgets.fmt
      ~attr:A.(fg white ++ bg blue)
      " mtop | tasks:%-5s (r:%-4s) | dom:%-3d | evt:%-6s%s%s | ? help"
      (Fmt.str "%a" Stats.pp_count ntasks)
      (Fmt.str "%a" Stats.pp_count running)
      ndomains
      (Fmt.str "%a" Stats.pp_count g.counter)
      sort mode
  in
  let paused =
    if g.paused then
      Some
        (Nottui_widgets.string
           ~attr:A.(fg white ++ bg red ++ st bold)
           " PAUSED ")
    else None
  in
  let lost =
    if g.lost > 0 then
      Some
        (Nottui_widgets.fmt
           ~attr:A.(fg white ++ bg red ++ st bold)
           " LOST %d " g.lost)
    else None
  in
  let extras = List.filter_map Fun.id [ paused; lost ] in
  Ui.hcat (base :: extras)

let separator = Nottui_widgets.string ~attr:A.(fg (gray 8)) (String.make 80 '-')

let select_next ~order selected =
  match order with
  | [] -> None
  | first :: _ -> (
      match selected with
      | None -> Some first
      | Some uid ->
          let rec find = function
            | [] -> None
            | [ last ] when last = uid -> Some uid
            | x :: y :: _ when x = uid -> Some y
            | _ :: rest -> find rest
          in
          find order)

let select_prev ~order selected =
  match order with
  | [] -> None
  | _ -> (
      match selected with
      | None -> Some (List.hd (List.rev order))
      | Some uid -> (
          let rec find prev = function
            | [] -> None
            | x :: _ when x = uid -> Some prev
            | x :: rest -> find x rest
          in
          match order with first :: rest -> find first rest | [] -> None))

let tree_body r g ~compare =
  let open Lwd.Infix in
  let tasks =
    Tree.render ~compare r.G.tasks g.G.tree ~selected:(Lwd.get r.G.selected)
  in
  let domains = Domains.render g.G.domains r.G.domains in
  domains >>= fun d ->
  tasks >|= fun t -> Ui.vcat [ d; separator; t ]

let table_body r g ~compare =
  let open Lwd.Infix in
  let tbl =
    Table.render r.G.tasks g.G.tree ~selected:(Lwd.get r.G.selected) ~compare
  in
  let domains = Domains.render g.G.domains r.G.domains in
  domains >>= fun d ->
  tbl >|= fun t -> Ui.vcat [ d; separator; t ]

let detail_body r g uid =
  let open Lwd.Infix in
  Lwd.get r.G.tasks >|= fun () -> Detail.render g uid

let resources_body r g =
  let open Lwd.Infix in
  Resources.render r.G.tasks g.G.tree >|= fun b ->
  Ui.vcat
    [
      Nottui_widgets.string
        ~attr:A.(fg (gray 12))
        "Backspace: back  |  j/k: scroll"; Nottui_widgets.string ""; b
    ]

let help_body =
  let open Lwd.Infix in
  Lwd.pure () >|= fun () ->
  let h s = Nottui_widgets.string ~attr:A.(fg white ++ st bold) s in
  let k key desc =
    Ui.hcat
      [
        Nottui_widgets.fmt ~attr:A.(fg yellow) "  %-14s" key
      ; Nottui_widgets.string ~attr:A.(fg white) desc
      ]
  in
  Ui.vcat
    [
      h "Help"; Nottui_widgets.string ""; h "Navigation"
    ; k "j / \xe2\x86\x93" "select next task"
    ; k "k / \xe2\x86\x91" "select previous task"
    ; k "g / G" "top / bottom scroll"; k "PgUp / PgDn" "page scroll"
    ; k "Enter" "drill into selected task detail"
    ; k "Backspace / h" "back to previous view"; Nottui_widgets.string ""
    ; h "Views"; k "v" "toggle Tree / Table view"
    ; k "r" "toggle Resources panel"; Nottui_widgets.string ""; h "Controls"
    ; k "Space" "pause / resume UI refresh"
    ; k "s" "cycle sort mode (uid/busy/polls/wakes/state)"
    ; k "i" "invert sort direction"; k "? / Esc" "toggle this help"
    ]

let root r g =
  let open Lwd.Infix in
  let scroll_y = Lwd.var 0 in
  let viewport_h = ref 0 in
  let content_h = ref 0 in
  let comparator () =
    Table.comparator (Lwd.peek r.G.sort_mode) (Lwd.peek r.G.sort_desc)
  in
  let current_order () =
    match Lwd.peek r.G.ui_mode with
    | G.Table -> Table.visible_order ~compare:(comparator ()) g.G.tree
    | Tree | Detail _ | Resources ->
        Tree.visible_order ~compare:(comparator ()) g.G.tree
  in
  let body =
    Lwd.get r.G.show_help >>= function
    | true -> help_body
    | false -> begin
        Lwd.get r.G.ui_mode >>= fun m ->
        Lwd.get r.G.sort_mode >>= fun sm ->
        Lwd.get r.G.sort_desc >>= fun sd ->
        let compare = Table.comparator sm sd in
        match m with
        | G.Tree -> tree_body r g ~compare
        | Table -> table_body r g ~compare
        | Detail uid -> detail_body r g uid
        | Resources -> resources_body r g
      end
  in
  let status = bar r g in
  body >>= fun body ->
  Lwd.get scroll_y >>= fun y ->
  status >|= fun status ->
  content_h := (Ui.layout_spec body).Ui.h;
  let body =
    body
    |> Ui.shift_area 0 y
    |> Ui.resize ~sh:1 ~h:0
    |> Ui.size_sensor (fun ~w:_ ~h -> viewport_h := h)
  in
  let ui = Ui.join_y body (Ui.resize ~w:80 ~sw:1 status) in
  let bound () = Int.max 0 (!content_h - !viewport_h) in
  let set y = Lwd.set scroll_y (Int.max 0 (Int.min (bound ()) y)) in
  let move_sel f =
    let cur = Lwd.peek r.G.selected in
    let order = current_order () in
    match f ~order cur with
    | None -> ()
    | Some uid -> Lwd.set r.G.selected (Some uid)
  in
  let back () =
    if Lwd.peek r.G.show_help then begin
      Lwd.set r.G.show_help false;
      Lwd.set scroll_y 0;
      true
    end
    else
      match Lwd.peek r.G.ui_mode with
      | Detail _ | Resources | Table ->
          Lwd.set r.G.ui_mode Tree; Lwd.set scroll_y 0; true
      | Tree -> false
  in
  let fn = function
    | `Arrow `Up, [] | `ASCII 'k', [] ->
        (match (Lwd.peek r.G.ui_mode, Lwd.peek r.G.show_help) with
        | (Tree | Table), false -> move_sel select_prev
        | _ -> set (Lwd.peek scroll_y - 1));
        `Handled
    | `Arrow `Down, [] | `ASCII 'j', [] ->
        (match (Lwd.peek r.G.ui_mode, Lwd.peek r.G.show_help) with
        | (Tree | Table), false -> move_sel select_next
        | _ -> set (Lwd.peek scroll_y + 1));
        `Handled
    | `Page `Up, [] ->
        set (Lwd.peek scroll_y - Int.max 1 (!viewport_h - 2));
        `Handled
    | `Page `Down, [] ->
        set (Lwd.peek scroll_y + Int.max 1 (!viewport_h - 2));
        `Handled
    | `ASCII 'g', [] -> set 0; `Handled
    | `ASCII 'G', [] ->
        set (bound ());
        `Handled
    | `Enter, [] ->
        (match
           (Lwd.peek r.G.ui_mode, Lwd.peek r.G.selected, Lwd.peek r.G.show_help)
         with
        | (Tree | Table), Some uid, false ->
            Lwd.set r.G.ui_mode (Detail uid);
            Lwd.set scroll_y 0
        | _ -> ());
        `Handled
    | `Backspace, [] | `ASCII 'h', [] ->
        if back () then `Handled else `Unhandled
    | `ASCII ' ', [] ->
        g.G.paused <- not g.G.paused;
        G.notify r;
        `Handled
    | `ASCII '?', [] ->
        Lwd.set r.G.show_help (not (Lwd.peek r.G.show_help));
        Lwd.set scroll_y 0;
        `Handled
    | `Escape, [] ->
        if Lwd.peek r.G.show_help then begin
          Lwd.set r.G.show_help false;
          Lwd.set scroll_y 0;
          `Handled
        end
        else `Unhandled
    | `ASCII 's', [] ->
        Lwd.set r.G.sort_mode (G.cycle_sort (Lwd.peek r.G.sort_mode));
        `Handled
    | `ASCII 'i', [] ->
        Lwd.set r.G.sort_desc (not (Lwd.peek r.G.sort_desc));
        `Handled
    | `ASCII 'r', [] ->
        (match Lwd.peek r.G.ui_mode with
        | Resources -> Lwd.set r.G.ui_mode Tree
        | _ -> Lwd.set r.G.ui_mode Resources);
        Lwd.set scroll_y 0; `Handled
    | `ASCII 'v', [] ->
        (match Lwd.peek r.G.ui_mode with
        | Tree -> Lwd.set r.G.ui_mode Table
        | Table -> Lwd.set r.G.ui_mode Tree
        | _ -> ());
        Lwd.set scroll_y 0; `Handled
    | _ -> `Unhandled
  in
  Ui.keyboard_area fn ui
