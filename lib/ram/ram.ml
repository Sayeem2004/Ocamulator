open UInt8
open UInt16

module RAM = struct
    type t = {
        max_mem : int;
        ram_memory : uint8 array;
    }

    let nes_zero_ram =
        { max_mem = 0xFFFF + 1; ram_memory = Array.init (0xFFF + 1) UInt8.from_int }

    let read_ui8 (ram : t) (addr : uint16) : uint8 =
        Array.get ram.ram_memory (UInt16.to_int addr)

    let read_ui16 (ram : t) (addr : uint16) : uint16 =
        let ui8_1 = Array.get ram.ram_memory (UInt16.to_int addr) in
        let ui8_2 = Array.get ram.ram_memory (UInt16.to_int addr + 1) in
        UInt16.ui16_combine_ui8 ui8_2 ui8_1

    let write_ui8 (ram : t) (addr : uint16) (value : uint8) : unit =
        Array.set ram.ram_memory (UInt16.to_int addr) value

    let write_ui16 (ram : t) (addr : uint16) (value : uint16) : unit =
        Array.set ram.ram_memory (UInt16.to_int addr)
            (UInt16.to_int value |> UInt8.from_int);
        Array.set ram.ram_memory
            (UInt16.to_int addr + 1)
            (UInt16.shift_right value 8 |> UInt16.to_int |> UInt8.from_int)
end
