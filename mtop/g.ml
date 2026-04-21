type sort = Uid | Busy | Polls | Wakes | State
type ui = Tree | Table | Detail of int | Resources

type react = {
    tasks: unit Lwd.var
  ; domains: unit Lwd.var
  ; selected: int option Lwd.var
  ; ui_mode: ui Lwd.var
  ; show_help: bool Lwd.var
  ; sort_mode: sort Lwd.var
  ; sort_desc: bool Lwd.var
}

type t = {
    tree: Tree.t
  ; domains: Domains.t
  ; mutable lost: int
  ; mutable counter: int
  ; mutable paused: bool
}

let r () =
  {
    tasks= Lwd.var ()
  ; domains= Lwd.var ()
  ; selected= Lwd.var None
  ; ui_mode= Lwd.var Tree
  ; show_help= Lwd.var false
  ; sort_mode= Lwd.var Uid
  ; sort_desc= Lwd.var false
  }

let create () =
  {
    tree= Hashtbl.create 0x7ff
  ; domains= Hashtbl.create 0x7ff
  ; lost= 0
  ; counter= 0
  ; paused= false
  }

let sort_mode_to_string = function
  | Uid -> "uid"
  | Busy -> "busy"
  | Polls -> "polls"
  | Wakes -> "wakes"
  | State -> "state"

let cycle_sort = function
  | Uid -> Busy
  | Busy -> Polls
  | Polls -> Wakes
  | Wakes -> State
  | State -> Uid

let notify (r : react) = Lwd.set r.tasks (); Lwd.set r.domains ()
