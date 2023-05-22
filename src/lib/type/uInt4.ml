module UInt4Set : UIntX.USet with type t := int = struct
  let max : int = 0xF
  let zero : int = 0
  let one : int = 1
  let size : int = 4
end

module UInt4 : UIntX.UInt = UIntX.SetToInt (UInt4Set)
include UInt4