open UInt8
open UInt16

type _ uint =
    | U8 : uint8 -> uint8 uint
    | U16 : uint16 -> uint16 uint

let ( ++ ) (type a) (u1 : a uint) (u2 : a uint) : a =
    match (u1, u2) with
    | U8 u8_1, U8 u8_2 -> UInt8.add u8_1 u8_2
    | U8 u8_1, U16 u16_2 -> UInt16.ui16_from_ui8 u8_1 |> UInt16.add u16_2
    | U16 u16_1, U8 u8_2 -> UInt16.ui16_from_ui8 u8_2 |> UInt16.add u16_1
    | U16 u16_1, U16 u16_2 -> UInt16.add u16_1 u16_2
