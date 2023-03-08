open UInt8;;
open UInt16;;

module RAM = struct
    type t = {
        max_mem : int;
        ram_memory : uint8 array;
    };;

    let nes_zero_ram = {
        max_mem = 0xFFFF + 1;
        ram_memory = Array.init (0xFFF + 1) UInt8.from_int;
    };;

    let read_ui8 (ram : t) (addr: uint16) : uint8 =
        Array.get ram.ram_memory (UInt16.to_int addr)
    ;;

    let read_ui16 (ram : t) (addr: uint16) : uint16 =
        let ui8_1 = Array.get ram.ram_memory (UInt16.to_int addr) in
        let ui8_2 = Array.get ram.ram_memory (UInt16.to_int addr + 1) in
        UInt16.ui16_combine_ui8 ui8_1 ui8_2
    ;;
end
