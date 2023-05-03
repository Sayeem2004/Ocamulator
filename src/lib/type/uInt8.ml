module UInt8Set : UIntX.USet with type t := int = struct
    let max : int = 0xFF
    let zero : int = 0
    let one : int = 1
    let size : int = 8
end

module UInt8 : UIntX.UInt = UIntX.SetToInt (UInt8Set)
include UInt8
