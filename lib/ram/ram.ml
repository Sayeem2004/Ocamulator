open UInt8
open UInt16
open UIntVar.UIntVar

module RAM = struct
    type t = {
        max_mem : int;
        ram_memory : uint8 array;
    };;

    let nes_zero_ram = {
        max_mem = 0xFFFF + 1;
        ram_memory = Array.init (0xFFF + 1) UInt8.from_int;
    };;

    let read (ram: t) (addr: uint_var) : uint_var =
        match addr with
        | U8 _ -> U8 (Array.get ram.ram_memory (to_int addr))
        | U16 _ ->
            let ui8_1 = Array.get ram.ram_memory (to_int addr) in
            let ui8_2 = Array.get ram.ram_memory (to_int addr + 1) in
            U16 (UInt16.ui16_combine_ui8 ui8_2 ui8_1)
        | _ -> raise (UnknownOperation "ram.ml memory address neither u8 or u16")
    ;;

    let write_ui8 (ram: t) (addr: uint16) (value: uint8) : unit =
        Array.set ram.ram_memory (UInt16.to_int addr) value
    ;;

    let write_ui16 (ram: t) (addr: uint16) (value: uint16) : unit =
        Array.set ram.ram_memory (UInt16.to_int addr)
            (UInt16.to_int value |> UInt8.from_int);
        Array.set ram.ram_memory (UInt16.to_int addr + 1)
            (UInt16.shift_right value 8 |> UInt16.to_int |> UInt8.from_int);
    ;;
end
