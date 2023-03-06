open Lib__UInt8
open Lib__UInt16

module RAM = struct
    let (max_mem: int) = 0xFFFF + 1;;
    let (ram_memory: uint8 array) = Array.init max_mem UInt8.from_int;;

    let read_ui8 (addr: uint16) : uint8 =
        Array.get ram_memory (UInt16.to_int addr)
    ;;
    let read_ui16 (addr: uint16) : uint16 =
        let ui8_1 = Array.get ram_memory (UInt16.to_int addr) in
        let ui8_2 = Array.get ram_memory (UInt16.to_int addr + 1) in
        UInt16.ui16_combine_ui8 ui8_1 ui8_2
    ;;
end
