open UInt8
open UInt16

module CPU = struct
    let accumulator : uint8 ref = ref UInt8.zero
    let register_X : uint8 ref = ref UInt8.zero
    let register_Y : uint8 ref = ref UInt8.zero
    let program_flag : uint8 ref = ref UInt8.zero
    let program_counter : uint16 ref = ref UInt16.zero
end
