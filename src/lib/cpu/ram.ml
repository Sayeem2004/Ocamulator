open UInt8
open UInt16

module RAM = struct
    type t = {
        max_mem : int;
        ram_memory : bytes;
    }

    let nes_zero_ram () : t =
        { max_mem = 0xFFFF + 1; ram_memory = Bytes.make (0xFFFF + 1) '\x00' }

    let nes_ram (rom_file : bytes) =
        { max_mem = 0xFFFF + 1; ram_memory = rom_file }

    let read_ui8 (ram : t) (addr : uint16) : uint8 =
        Bytes.get_uint8 ram.ram_memory (UInt16.to_int addr) |> ( ~. )

    let write_ui8 (ram : t) (addr : uint16) (value : uint8) : unit =
        Bytes.set_uint8 ram.ram_memory (UInt16.to_int addr) (UInt8.to_int value)
end
