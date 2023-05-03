module type USet = sig
    type t

    val max : t
    val zero : t
    val one : t
    val size : int
end

module type UInt = sig
    type t

    val max : t
    val zero : t
    val one : t
    val size : int
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val rem : t -> t -> t
    val succ : t -> t
    val pred : t -> t
    val logand : t -> t -> t
    val logor : t -> t -> t
    val logxor : t -> t -> t
    val left : t -> int -> t
    val right : t -> int -> t
    val from_int : int -> t
    val to_int : t -> int
    val to_string : t -> string
end

module SetToInt (M : USet with type t := int) : UInt = struct
    type t = int

    let max : t = M.max
    let zero : t = M.zero
    let one : t = M.one
    let size : int = M.size
    let compare (ui1 : t) (ui2 : t) : int = Stdlib.compare ui1 ui2
    let equal (ui1 : t) (ui2 : t) : bool = compare ui1 ui2 = 0
    let add (ui1 : t) (ui2 : t) : t = (ui1 + ui2) land max
    let sub (ui1 : t) (ui2 : t) : t = (ui1 - ui2) land max
    let mul (ui1 : t) (ui2 : t) : t = ui1 * ui2 land max
    let div (ui1 : t) (ui2 : t) : t = ui1 / ui2 land max
    let rem (ui1 : t) (ui2 : t) : t = ui1 mod ui2 land max
    let succ (ui : t) : t = add ui one
    let pred (ui : t) : t = sub ui one
    let logand (ui1 : t) (ui2 : t) : t = ui1 land ui2
    let logor (ui1 : t) (ui2 : t) : t = ui1 lor ui2
    let logxor (ui1 : t) (ui2 : t) = ui1 lxor ui2
    let left (ui1 : t) (i : int) : t = (ui1 lsl i) land max
    let right (ui1 : t) (i : int) : t = ui1 lsr i
    let from_int (i : int) : t = i land max
    let to_int (ui : t) : int = ui
    let to_string (ui : t) : string = Printf.sprintf "$%04X" ui
end
