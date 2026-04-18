open Notty
open Nottui

let bar r g =
  let open Lwd.Infix in
  Lwd.get r.G.tasks >>= fun () ->
  Lwd.get r.G.only_active >|= fun only_active ->
  let ntasks = Hashtbl.length g.G.tree in
  let ndomains = Hashtbl.length g.G.domains in
  let running =
    let fn _ task acc =
      if task.Task.state = State.Running then acc + 1 else acc
    in
    Hashtbl.fold fn g.tree 0
  in
  let lost = if g.lost > 0 then Fmt.str " | Lost: %d" g.lost else "" in
  let clean = if only_active then " [clean]" else "" in
  Nottui_widgets.fmt
    ~attr:A.(fg white ++ bg blue)
    " mtop | tasks: %d (running: %d) | domains: %d | events: %d%s%s | c:clean \
     ESC:quit"
    ntasks running ndomains g.counter lost clean

let separator =
  Lwd.pure (Nottui_widgets.string ~attr:A.(fg (gray 8)) (String.make 80 '-'))

let root r g =
  let open Lwd.Infix in
  let only_active = Lwd.var false in
  let tasks = Tree.render ~only_active:r.G.only_active r.G.tasks g.G.tree in
  let domains = Domains.render g.domains r.domains in
  let stats = Stats.render ~only_active:r.G.only_active r.G.tasks g.G.tree in
  let status = bar r g in
  let body =
    Nottui_widgets.vbox [ tasks; separator; domains; separator; stats ]
  in
  let scroll = Nottui_widgets.scrollbox body in
  scroll >>= fun body ->
  status >|= fun status ->
  let body = Ui.resize ~sh:1 body in
  let ui = Ui.join_y body (Ui.resize ~w:80 ~sw:1 status) in
  let fn = function
    | `ASCII 'c', [] ->
        Lwd.set only_active (not (Lwd.peek only_active));
        `Handled
    | _ -> `Unhandled
  in
  Ui.keyboard_area fn ui
