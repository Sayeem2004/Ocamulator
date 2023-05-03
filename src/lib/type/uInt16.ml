module UInt16Set : UIntX.USet with type t := int = struct
    let max : int = 0xFFFF
    let zero : int = 0
    let one : int = 1
    let size : int = 16
end

module UInt16 : UIntX.UInt = UIntX.SetToInt (UInt16Set)
include UInt16

let ui16_from_ui8 (ui8 : UInt8.t) : t = from_int (UInt8.to_int ui8)

let combine_ui8 (hig : UInt8.t) (low : UInt8.t) : t =
    left (ui16_from_ui8 hig) 8 |> add (ui16_from_ui8 low)

let split_ui16 (u : t) : UInt8.t * UInt8.t =
    (right u 8 |> to_int |> UInt8.from_int, u |> to_int |> UInt8.from_int)
