module UInt8Set : UIntSet.S with type t := int = struct
    let (max_value: int) = 0xFF;;
    let (zero: int) = 0;;
    let (one: int) = 1;;
    let (size: int) = 8;;
end

module UInt8 = UIntX.F(UInt8Set);;

type uint8 = UInt8.t;;
