open Lib__UInt8
open Lib__UInt16

module CPU = struct
    let accumulator : uint8 ref = ref UInt8.zero
    let register_X : uint8 ref = ref UInt8.zero
    let register_Y : uint8 ref = ref UInt8.zero
    let program_counter : uint16 ref = ref UInt16.zero

    let carry_bit : bool ref = ref false
    let zero : bool ref = ref false
    let interrupt : bool ref = ref false
    let decimal : bool ref = ref false
    let negative : bool ref = ref false
    let overflow : bool ref = ref false
    let break : bool ref = ref false
    let reserved : bool ref = ref false

    type memory_mode =
    | Accumulator
    | Absolute
    | AbsoluteX
    | AbsoluteY
    | Immediate
    | Implied
    | Indirect
    | XIndirect
    | YIndirect
    | Relative
    | Zeropage
    | ZeropageX
    | ZeropageY

    let clc = carry_bit := false
    let cld = decimal := false
    let cli = interrupt := false
    let clv = overflow := false
end
