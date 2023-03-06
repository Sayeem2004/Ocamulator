open Lib__UInt8
open Lib__UInt16

module RAM = struct
    let max_mem = 0xFFFF + 1
    let ram_memory = Array.init max_mem UInt8.from_int

    let read_ui8 (addr : uint16) : uint8 =
        Array.get ram_memory (UInt16.to_int addr)

    let read_ui16 (addr : uint16) : uint16 =
        let u81 = Array.get ram_memory (UInt16.to_int addr) in
        let u82 = Array.get ram_memory (UInt16.to_int addr + 1) in
        UInt16.ui16_combine_ui8 u81 u82
end
