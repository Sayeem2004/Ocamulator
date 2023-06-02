type interrupt =
    | NMI
    | IRQ
    | RESET

type joypad_button =
    | Right
    | Left
    | Down
    | Up
    | Start
    | Select
    | Button_B
    | Button_A

let interrupt_to_string (interrupt : interrupt) =
    match interrupt with NMI -> "NMI" | IRQ -> "IRQ" | RESET -> "RESET"

type master =
    | E_B
    | E_C

type mirroring =
    | HORIZONTAL
    | VERTICAL
    | SINGLE
    | QUAD

type uint4 = UInt4.t
type uint8 = UInt8.t
type uint16 = UInt16.t

exception ReadError of uint16
exception WriteError of uint16
exception InvalidValue

let bit7_mask = UInt8.from_int 0b10000000
let bit6_mask = UInt8.from_int 0b01000000
let bit5_mask = UInt8.from_int 0b00100000
let bit4_mask = UInt8.from_int 0b00010000
let bit3_mask = UInt8.from_int 0b00001000
let bit2_mask = UInt8.from_int 0b00000100
let bit1_mask = UInt8.from_int 0b00000010
let bit0_mask = UInt8.from_int 0b00000001

let no_bit7_mask = UInt8.from_int 0b01111111
let no_bit6_mask = UInt8.from_int 0b10111111
let no_bit5_mask = UInt8.from_int 0b11011111
let no_bit4_mask = UInt8.from_int 0b11101111
let no_bit3_mask = UInt8.from_int 0b11110111
let no_bit2_mask = UInt8.from_int 0b11111011
let no_bit1_mask = UInt8.from_int 0b11111101
let no_bit0_mask = UInt8.from_int 0b11111110

let ( ~... ) = UInt4.from_int
let ( <?> ) = UInt8.compare
let ( <-> ) = UInt8.equal
let ( ++ ) = UInt8.add
let ( ** ) = UInt8.mul
let ( // ) = UInt8.div
let ( -- ) = UInt8.sub
let ( !. ) = UInt8.lognot
let ( &&. ) = UInt8.logand
let ( |&. ) = UInt8.logxor
let ( ||. ) = UInt8.logor
let ( << ) = UInt8.left
let ( >> ) = UInt8.right
let ( ~. ) = UInt8.from_int
let ( ~* ) = UInt8.to_int
let ( ?. ) (b : bool) = Bool.to_int b |> UInt8.from_int
let ( ?* ) (u : uint8) = u <?> UInt8.zero = 0
let ( ?- ) (u : uint8) = not (u &&. ~.0x80 <-> ~.0x00)
let ( ?+ ) (u : uint8) = not (u &&. ~.0x01 <-> ~.0x00)
let ( ?@ ) (u : uint8) = u |&. ~.0xFF
let ( ?% ) (u : uint8) = if ?-u then -(256 - ~*u) else ~*u
let ( ?& ) (ui8 : uint8) (n : int) = match n with
| 7 -> ui8 &&. bit7_mask <-> UInt8.zero
| 6 -> ui8 &&. bit6_mask <-> UInt8.zero
| 5 -> ui8 &&. bit5_mask <-> UInt8.zero
| 4 -> ui8 &&. bit4_mask <-> UInt8.zero
| 3 -> ui8 &&. bit3_mask <-> UInt8.zero
| 2 -> ui8 &&. bit2_mask <-> UInt8.zero
| 1 -> ui8 &&. bit1_mask <-> UInt8.zero
| 0 -> ui8 &&. bit0_mask <-> UInt8.zero
| _ -> raise InvalidValue
let ( !& ) (ui8 : uint8) (n : int) (v : bool) = match n with
| 7 -> if v then ui8 ||. bit7_mask else ui8 &&. no_bit7_mask
| 6 -> if v then ui8 ||. bit6_mask else ui8 &&. no_bit6_mask
| 5 -> if v then ui8 ||. bit5_mask else ui8 &&. no_bit5_mask
| 4 -> if v then ui8 ||. bit4_mask else ui8 &&. no_bit4_mask
| 3 -> if v then ui8 ||. bit3_mask else ui8 &&. no_bit3_mask
| 2 -> if v then ui8 ||. bit2_mask else ui8 &&. no_bit2_mask
| 1 -> if v then ui8 ||. bit1_mask else ui8 &&. no_bit1_mask
| 0 -> if v then ui8 ||. bit0_mask else ui8 &&. no_bit0_mask
| _ -> raise InvalidValue
let ( /> ) (u1 : uint8) (u2 : uint8) = u1 <?> u2 > 0
let ( />= ) (u1 : uint8) (u2 : uint8) = u1 <?> u2 >= 0
let ( /< ) (u1 : uint8) (u2 : uint8) = u1 <?> u2 < 0
let ( /<= ) (u1 : uint8) (u2 : uint8) = u1 <?> u2 <= 0
let ( <??> ) = UInt16.compare
let ( <--> ) = UInt16.equal
let ( +++ ) = UInt16.add
let ( --- ) = UInt16.sub
let ( *** ) = UInt16.mul
let ( /// ) = UInt16.div
let ( &&& ) = UInt16.logand
let ( ||| ) = UInt16.logor
let ( <<< ) = UInt16.left
let ( >>> ) = UInt16.right
let ( ~.. ) = UInt16.from_int
let ( ~** ) = UInt16.to_int
let ( !... ) = UInt16.lognot
let ( !.. ) = UInt16.combine_ui8
let ( !** ) = UInt16.ui16_from_ui8
let ( !*** ) u = UInt16.to_int u |> UInt8.from_int
let ( !-- ) u = UInt16.to_int u |> UInt8.from_int
let ( !++ ) b = Bool.to_int b |> UInt16.from_int
let ( !@@ ) = UInt16.split_ui16
let ( //> ) (u1 : uint16) (u2 : uint16) = u1 <??> u2 > 0
let ( //>= ) (u1 : uint16) (u2 : uint16) = u1 <??> u2 >= 0
let ( //< ) (u1 : uint16) (u2 : uint16) = u1 <??> u2 < 0
let ( //<= ) (u1 : uint16) (u2 : uint16) = u1 <??> u2 <= 0
