type t =
  | Idle
  | Running
  | Yielded
  | Waking
  | Suspended of string
  | Awaiting
  | Cancelled
  | Finished

let to_string = function
  | Idle -> "idle"
  | Running -> "running"
  | Yielded -> "yielded"
  | Waking -> "waking"
  | Suspended name -> Fmt.str "s:%s" name
  | Awaiting -> "awaiting"
  | Cancelled -> "cancelled"
  | Finished -> "finished"

let rank = function
  | Running -> 0
  | Waking -> 1
  | Yielded -> 2
  | Suspended _ -> 3
  | Awaiting -> 4
  | Idle -> 5
  | Finished -> 6
  | Cancelled -> 7

let glyph = function
  | Idle -> "\xe2\x97\x8b" (* ○ U+25CB *)
  | Running -> "\xe2\x96\xb8" (* ▸ U+25B8 *)
  | Yielded -> "\xe2\x97\x87" (* ◇ U+25C7 *)
  | Waking -> "\xe2\x86\x91" (* ↑ U+2191 *)
  | Suspended _ -> "\xe2\x97\x8c" (* ◌ U+25CC *)
  | Awaiting -> "\xe2\x97\x8b" (* ○ U+25CB *)
  | Cancelled -> "\xc3\x97" (* × U+00D7 *)
  | Finished -> "\xe2\x96\xa0" (* ■ U+25A0 *)

open Notty

let attr = function
  | Idle -> A.(fg (gray 16))
  | Running -> A.(fg green)
  | Yielded -> A.(fg (gray 14))
  | Waking -> A.(fg cyan)
  | Suspended _ -> A.(fg yellow)
  | Awaiting -> A.(fg yellow)
  | Cancelled -> A.(fg red)
  | Finished -> A.(fg (gray 8))
