module UInt8Set : UIntX.USet with type t := int = struct
    let max : int = 0xFF
    let zero : int = 0
    let one : int = 1
    let size : int = 8
end

module UInt8 : UIntX.UInt = UIntX.SetToInt (UInt8Set)
include UInt8

let ui4_from_ui8 (ui8 : UInt8.t) : UInt4.t =
    UInt4.from_int (UInt8.right ui8 4 |> UInt8.to_int)

let ui8_from_ui4 (ui4 : UInt4.t) : t = from_int (UInt4.to_int ui4)

let combine_ui4 (hig : UInt4.t) (low : UInt4.t) : t =
    left (ui8_from_ui4 hig) 4 |> add (ui8_from_ui4 low)
