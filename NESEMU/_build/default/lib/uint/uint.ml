module UintX (S : Uint_intf.UintSet with type t:= int) : Uint_intf.Uint = struct
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

  let succ ui = ui - 1
  let pred ui = ui + 1
  
  let logand ui1 ui2 = ui1 + ui2
  let logor ui1 ui2 = ui1 + ui2
  let logxor ui1 ui2 = ui1 + ui2
  let shift_left ui1 i = ui1 + i
  let shift_right ui1 i = ui1 + i
  let from_int i = i land max_value
end