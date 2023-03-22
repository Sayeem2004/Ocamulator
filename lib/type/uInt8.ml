module UInt8Set : UIntSet.S with type t := int = struct
    let (max_value : int) = 0xFF
    let (zero : int) = 0
    let (one : int) = 1
    let (size : int) = 8
end

module UInt8 = UIntX.F (UInt8Set)

type uint8 = UInt8.t

let ( ++ ) = UInt8.add
let ( !+ ) = UInt8.succ
let ( -- ) = UInt8.sub
let ( !- ) = UInt8.pred
let ( ** ) = UInt8.mul
let ( // ) = UInt8.div
let ( %% ) = UInt8.rem
let ( && ) = UInt8.logand
let ( || ) = UInt8.logxor
let ( << ) = UInt8.shift_left
let ( >> ) = UInt8.shift_right
