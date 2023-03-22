open UInt8
open UInt16

module RAM = struct
    type t = {
        max_mem : int;
        ram_memory : uint8 array;
    }

    let nes_zero_ram =
        {
            max_mem = 0xFFFF + 1;
            ram_memory = Array.init (0xFFFF + 1) (fun x -> UInt8.zero);
        }

    let read_ui8 (ram : t) (addr : uint16) : uint8 =
        Array.get ram.ram_memory (UInt16.to_int addr)

    let read_ui16 (ram : t) (addr : uint16) : uint16 =
        let mem_addr_int = UInt16.to_int addr in
        let least_sig_bit = Array.get ram.ram_memory mem_addr_int in
        let most_sig_bit = Array.get ram.ram_memory (mem_addr_int + 1) in
        UInt16.ui16_combine_ui8 least_sig_bit most_sig_bit

    let write_ui8 (ram : t) (addr : uint16) (value : uint8) : unit =
        Array.set ram.ram_memory (UInt16.to_int addr) value

    let write_ui16 (ram : t) (addr : uint16) (value : uint16) : unit =
        let mem_addr_int = UInt16.to_int addr in
        let least_sig_bit = !. value in
        let most_sig_bit = !. (value >>> 8) in
        Array.set ram.ram_memory mem_addr_int least_sig_bit;
        Array.set ram.ram_memory (mem_addr_int + 1) most_sig_bit
end
