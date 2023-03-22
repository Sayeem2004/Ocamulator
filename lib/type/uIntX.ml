module F (M : UIntSet.S with type t := int) : UInt.S = struct
    type t = int

    let to_string (ui : t) : string = Printf.sprintf "$%04X" ui
    let max_value : t = M.max_value
    let zero : t = M.zero
    let one : t = M.one
    let size : int = M.size
    let compare (ui1 : t) (ui2 : t) : int = Stdlib.compare ui1 ui2
    let equal (ui1 : t) (ui2 : t) : bool = compare ui1 ui2 = 0
    let add (ui1 : t) (ui2 : t) : t = (ui1 + ui2) land max_value
    let sub (ui1 : t) (ui2 : t) : t = (ui1 - ui2) land max_value
    let mul (ui1 : t) (ui2 : t) : t = ui1 * ui2 land max_value
    let div (ui1 : t) (ui2 : t) : t = ui1 / ui2 land max_value
    let rem (ui1 : t) (ui2 : t) : t = ui1 mod ui2 land max_value
    let succ (ui : t) : t = add ui one
    let pred (ui : t) : t = sub ui one
    let logand (ui1 : t) (ui2 : t) : t = ui1 land ui2
    let logor (ui1 : t) (ui2 : t) : t = ui1 lor ui2
    let logxor (ui1 : t) (ui2 : t) = ui1 lxor ui2
    let shift_left (ui1 : t) (i : int) : t = (ui1 lsl i) land max_value
    let shift_right (ui1 : t) (i : int) : t = ui1 lsr i
    let from_int (i : int) : t = i land max_value
    let to_int (ui : t) : int = ui
end
