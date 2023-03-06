module UInt16Set : UIntSet.S with type t := int = struct
    let max_value = 0xFFFF
    let zero = 0
    let one = 1
end

open Lib__UInt8

module UInt16 = struct
    include UIntX.F(UInt16Set)

    let ui16_from_ui8 ui8 = from_int (UInt8.of_int ui8)
    let ui16_combine_ui8 (ui8_1: uint8) (ui8_2: uint8) : t =
        shift_left (ui16_from_ui8 ui8_1) 8 |> add (ui16_from_ui8 ui8_2)
end
type uint16 = UInt16.t
