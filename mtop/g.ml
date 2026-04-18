type react = {
    tasks: unit Lwd.var
  ; domains: unit Lwd.var
  ; only_active: bool Lwd.var
}

type t = {
    tree: Tree.t
  ; domains: Domains.t
  ; mutable lost: int
  ; mutable counter: int
}

let r () = { tasks= Lwd.var (); domains= Lwd.var (); only_active= Lwd.var true }

let create () =
  {
    tree= Hashtbl.create 0x7ff
  ; domains= Hashtbl.create 0x7ff
  ; lost= 0
  ; counter= 0
  }

let notify (r : react) = Lwd.set r.tasks (); Lwd.set r.domains ()
