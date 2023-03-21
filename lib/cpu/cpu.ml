open Ram
open UIntVar.UIntVar

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
        accumulator: uint_var;
        register_X: uint_var;
        register_Y: uint_var;
        program_counter: uint_var;
        ram: RAM.t;
        flags: cpu_flags;
    };;

    let fetch (cpu: t) (addr: uint_var) : uint_var = RAM.read cpu.ram addr;;
    (* let write_ui8 (cpu : t) (addr : uint16) (value : uint8) = RAM.write_ui8 cpu.ram addr value;;
    let write_ui16 (cpu : t) (addr : uint16) (value : uint16) = RAM.write_ui16 cpu.ram addr value;;*)
end
