module UintX (S : Uint_intf.UintSet with type t := int) : Uint_intf.Uint = struct
  type t = int

  let to_string ui = Printf.sprintf "$%04X" ui

  let max_value = S.max_value
  let zero = S.zero
  let one = S.one

  let compare ui1 ui2 = Int.compare ui1 ui2
  let equal ui1 ui2 = compare ui1 ui2 == 0

  let le ui1 ui2 = ui1 <= ui2
  let add ui1 ui2 = (ui1 + ui2) land max_value
  let sub ui1 ui2 = (ui1 - ui2) land max_value
  let mul ui1 ui2 = (ui1 * ui2) land max_value
  let div ui1 ui2 = (ui1 / ui2) land max_value
  let rem ui1 ui2 = (ui1 mod ui2) land max_value

  let succ ui = add ui one
  let pred ui = sub ui one
  
  let logand ui1 ui2 = ui1 land ui2
  let logor ui1 ui2 = ui1 lor ui2
  let logxor ui1 ui2 = ui1 lxor ui2
  let shift_left ui1 i = ui1 lsl i
  let shift_right ui1 i = ui1 lsr i
  let from_int i = i land max_value
end

module Uint8Set : Uint_intf.UintSet with type t:= int = struct
  let max_value = 0xFF
  let zero = 0
  let one = 1
end

module Uint16Set : Uint_intf.UintSet with type t:= int = struct
  let max_value = 0xFFFF
  let zero = 0
  let one = 1
end

module Uint8 = UintX(Uint8Set)
module Uint16 = UintX(Uint16Set)

type uint8 = Uint8.t
type uint16 = Uint16.t