type t =
  | Idle
  | Running
  | Suspended of string
  | Awaiting
  | Cancelled
  | Finished

let to_string = function
  | Idle -> "idle"
  | Running -> "running"
  | Suspended name -> Fmt.str "s:%s" name
  | Awaiting -> "awaiting"
  | Cancelled -> "cancelled"
  | Finished -> "finished"

open Notty

let attr = function
  | Idle -> A.(fg (gray 16))
  | Running -> A.(fg green)
  | Suspended _ -> A.(fg yellow)
  | Awaiting -> A.(fg yellow)
  | Cancelled -> A.(fg red)
  | Finished -> A.(fg (gray 8))
