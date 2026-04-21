open Notty
open Nottui

let reverse_index (tree : Tree.t) =
  let tbl = Hashtbl.create 0x3f in
  let fn _ (task : Task.t) =
    List.iter
      (fun ruid ->
        let prior = try Hashtbl.find tbl ruid with Not_found -> [] in
        if not (List.mem task.uid prior) then
          Hashtbl.replace tbl ruid (task.uid :: prior))
      task.resources
  in
  Hashtbl.iter fn tree; tbl

let render_row ~now tree ruid holders =
  let holders = List.sort Int.compare holders in
  let head =
    Nottui_widgets.fmt
      ~attr:A.(fg white ++ st bold)
      "  resource #%d  (%d holder%s)" ruid (List.length holders)
      (if List.length holders = 1 then "" else "s")
  in
  let rows =
    List.map
      (fun uid ->
        match Tree.find tree uid with
        | None ->
            Nottui_widgets.fmt ~attr:A.(fg (gray 10)) "      #%-10d (gone)" uid
        | Some task ->
            let st = Task.effective_state ~now task in
            let loc =
              match task.location with
              | None -> ""
              | Some (f, l) -> Fmt.str "  %s:%d" f l
            in
            Nottui_widgets.fmt ~attr:(State.attr st) "      %s #%-10d %-10s%s"
              (State.glyph st) task.uid (State.to_string st) loc)
      holders
  in
  Ui.vcat (head :: rows)

let render tasks tree =
  let open Lwd.Infix in
  Lwd.get tasks >|= fun () ->
  let now = Unix.gettimeofday () in
  let idx = reverse_index tree in
  let pairs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) idx [] in
  let pairs = List.sort (fun (a, _) (b, _) -> Int.compare a b) pairs in
  let hdr =
    Nottui_widgets.string
      ~attr:A.(fg white ++ st bold)
      "Resources (ruid -> holders)"
  in
  match pairs with
  | [] ->
      Ui.vcat
        [
          hdr
        ; Nottui_widgets.string
            ~attr:A.(fg (gray 12))
            "  (no resources tracked)"
        ]
  | pairs ->
      let rows =
        List.map
          (fun (ruid, holders) -> render_row ~now tree ruid holders)
          pairs
      in
      let sep = Nottui_widgets.string "" in
      Ui.vcat (hdr :: sep :: List.concat_map (fun r -> [ r; sep ]) rows)
