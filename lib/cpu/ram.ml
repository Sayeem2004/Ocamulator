open UInt8
open UInt16

module RAM = struct
    type t = {
        max_mem : int;
        ram_memory : bytes;
    }

    let nes_zero_ram =
        { max_mem = 0xFFFF + 1; ram_memory = Bytes.make 0xFFFF ' ' }

    let nes_ram (rom : bytes) = { max_mem = 0xFFFF + 1; ram_memory = rom }
    let byte_to_uint8 (byte : char) = Char.code byte |> UInt8.from_int
    let uint8_to_byte (u8 : uint8) = UInt8.to_int u8 |> Char.chr

    let read_ui8 (ram : t) (addr : uint16) : uint8 =
        Bytes.get ram.ram_memory (UInt16.to_int addr) |> byte_to_uint8

    let read_ui16 (ram : t) (addr : uint16) : uint16 =
        let mem_addr_int = UInt16.to_int addr in
        let least_sig_bit =
            Bytes.get ram.ram_memory mem_addr_int |> byte_to_uint8
        in
        let most_sig_bit =
            Bytes.get ram.ram_memory (mem_addr_int + 1) |> byte_to_uint8
        in
        UInt16.ui16_combine_ui8 least_sig_bit most_sig_bit

    let write_ui8 (ram : t) (addr : uint16) (value : uint8) : unit =
        Bytes.set ram.ram_memory (UInt16.to_int addr) (uint8_to_byte value)

    let write_ui16 (ram : t) (addr : uint16) (value : uint16) : unit =
        let mem_addr_int = UInt16.to_int addr in
        let least_sig_bit = !.value in
        let most_sig_bit = !.(value >>> 8) in
        Bytes.set ram.ram_memory mem_addr_int (uint8_to_byte least_sig_bit);
        Bytes.set ram.ram_memory (mem_addr_int + 1) (uint8_to_byte most_sig_bit)
end
