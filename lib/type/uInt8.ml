module UInt8Set : UIntSet.S with type t := int = struct
    let max_value = 0xFF
    let zero = 0
    let one = 1
end

module UInt8 = struct
    include UIntX.F(UInt8Set)
end

type uint8 = UInt8.t
