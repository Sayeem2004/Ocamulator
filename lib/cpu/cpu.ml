open Lib__UInt8
open Lib__UInt16
open Lib__Ram

module CPU = struct
    type cpu_flags = {
        carr_bit: bool;
        zero: bool;
        interrupt: bool;
        decimal: bool;
        negative: bool;
        overflow: bool;
        break: bool;
        reserved: bool;
    };;

    type t = {
        accumulator: uint8;
        register_X: uint8;
        register_Y: uint8;
        program_counter: uint16;
        ram: RAM.t;
        flags: cpu_flags;
    };;

    let fetch_ui8 (cpu: t) (addr: uint16) : uint8 = RAM.read_ui8 cpu.ram addr;;
    let fetch_ui16 (cpu: t) (addr: uint16) : uint16 = RAM.read_ui16 cpu.ram addr;;
    let write_ui8 (cpu : t) (addr : uint16) (value : uint8) = RAM.write_ui8 cpu.ram addr value;;
    let write_ui16 (cpu : t) (addr : uint16) (value : uint16) = RAM.write_ui16 cpu.ram addr value;;
end
