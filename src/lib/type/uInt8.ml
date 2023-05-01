module UInt8Set : UIntX.USet with type t := int = struct
    let max_value : int = 0xFF
    let zero : int = 0
    let one : int = 1
    let size : int = 8
end

module UInt8 = UIntX.SetToInt (UInt8Set)

type uint8 = UInt8.t

let ( <?> ) = UInt8.compare
let ( <-> ) = UInt8.equal
let ( ++ ) = UInt8.add
let ( !+ ) = UInt8.succ
let ( -- ) = UInt8.sub
let ( !- ) = UInt8.pred
let ( ** ) = UInt8.mul
let ( // ) = UInt8.div
let ( %% ) = UInt8.rem
let ( &&. ) = UInt8.logand
let ( |/. ) = UInt8.logxor
let ( ||. ) = UInt8.logor
let ( << ) = UInt8.shift_left
let ( >> ) = UInt8.shift_right
let ( ~. ) = UInt8.from_int
let ( ?. ) i = Bool.to_int i |> UInt8.from_int
let ( ?> ) u8_1 u8_2 = u8_1 ++ u8_2 <?> u8_1 < 0
let ( ?- ) u8 = not (u8 &&. ~.0b10000000 <-> ~.0x00)
let ( ?* ) u8 = u8 <?> UInt8.zero = 0
let ( ?+ ) u8 = not (u8 &&. ~.0b00000001 <-> ~.0x00)

let ( ~--. ) u =
    let of_int = UInt8.to_int u in
    if ?-u then of_int - (2 * 0b10000000) else of_int
