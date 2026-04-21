type t = (int, Domain.t) Hashtbl.t

let get t uid =
  match Hashtbl.find_opt t uid with
  | Some domain -> domain
  | None ->
      let domain = Domain.create uid in
      Hashtbl.replace t uid domain;
      domain

let list t =
  let fn _ domain acc = domain :: acc in
  Hashtbl.fold fn t []

let tick ~now t =
  let fn _ t = Domain.tick ~now t in
  Hashtbl.iter fn t

open Notty
open Nottui

let render t domains =
  let open Lwd.Infix in
  Lwd.get domains >|= fun () ->
  let domains = list t in
  let domains = List.sort Domain.compare domains in
  let hdr =
    Nottui_widgets.string ~attr:A.(fg white ++ A.st bold) "Domain Utilization"
  in
  match domains with
  | [] ->
      let value =
        Nottui_widgets.string ~attr:A.(fg (gray 12)) "No domain activity yet..."
      in
      Ui.join_y hdr value
  | domains ->
      let rows = List.map Domain.render domains in
      Ui.vcat (hdr :: rows)
