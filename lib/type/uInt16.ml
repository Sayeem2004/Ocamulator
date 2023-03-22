open UInt8

module UInt16Set : UIntSet.S with type t := int = struct
    let (max_value : int) = 0xFFFF
    let (zero : int) = 0
    let (one : int) = 1
    let (size : int) = 16
end

module UInt16 = struct
    include UIntX.F (UInt16Set)

    let ui16_from_ui8 (ui8 : uint8) : t = from_int (UInt8.to_int ui8)

    let ui16_combine_ui8 (ui8_1 : uint8) (ui8_2 : uint8) : t =
        shift_left (ui16_from_ui8 ui8_1) 8 |> add (ui16_from_ui8 ui8_2)
end

type uint16 = UInt16.t;;

(* TODO Add prefix operators to
    1. Downcast from uint16 -> uint8
    2. Upcast from uint8 -> uint16
    Maybe?    !->    or !.
              !^
*)

let ( <?> ) u8_1 u8_2 = UInt8.compare u8_1 u8_2
let ( <??> ) u16_1 u16_2 = UInt16.compare u16_1 u16_2

let ( <-> ) u8_1 u8_2 = u8_1 <?> u8_2 = 0
let ( <--> ) u16_1 u16_2 = u16_1 <??> u16_2 = 0

let ( ~^ ) = UInt16.from_int
let ( ~. ) = UInt8.from_int

let ( ?* ) u8 = u8 <?> UInt8.zero = 0
let ( ?- ) u8 = not (?* (u8 &&. ~. (0b10000000)))

let ( ?^ ) i = Bool.to_int i |> UInt16.from_int
let ( ?. ) i = Bool.to_int i |> UInt8.from_int

let ( !^ ) = UInt16.ui16_from_ui8
let ( !. ) i = UInt16.to_int i |> UInt8.from_int

let ( <??> ) = UInt16.compare
let ( <--> ) = UInt16.equal
let ( +++ ) = UInt16.add
let ( !++ ) = UInt16.succ
let ( --- ) = UInt16.sub
let ( !-- ) = UInt16.pred
let ( *** ) = UInt16.mul
let ( /// ) = UInt16.div
let ( %%% ) = UInt16.rem
let ( &&& ) = UInt16.logand
let ( ||| ) = UInt16.logxor
let ( <<< ) = UInt16.shift_left
let ( >>> ) = UInt16.shift_right
let ( ~^ ) = UInt16.from_int
let ( ?^ ) i = Bool.to_int i |> UInt16.from_int
let ( ?. ) i = Bool.to_int i |> UInt8.from_int
let ( !^ ) = UInt16.ui16_from_ui8
let ( !. ) i = UInt16.to_int i |> UInt8.from_int
