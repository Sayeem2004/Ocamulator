open UInt8
open UInt16
open Ram

module CPU = struct
    type cpu_flags = {
        carr_bit : bool;
        zero : bool;
        interrupt : bool;
        decimal : bool;
        negative : bool;
        overflow : bool;
        break : bool;
        reserved : bool;
    }

    type t = {
        accumulator : uint8;
        register_X : uint8;
        register_Y : uint8;
        program_counter : uint16;
        stack_pointer : uint8;
        ram : RAM.t;
        flags : cpu_flags;
    }

    let nes_cpu (init_pc : uint16) (nes_ram : RAM.t): t =
        {
            accumulator = ~. 0;
            register_X = ~. 0;
            register_Y = ~. 0;
            program_counter = init_pc;
            stack_pointer = ~. 0xFF;
            ram = nes_ram;
            flags = {
                carr_bit = false;
                zero = false;
                interrupt = true;
                decimal = false;
                negative = false;
                overflow = false;
                break = false;
                reserved = true;
            }
        }

    let flags_ui8 (cpu : t) : uint8 =
        let neg_flag = ~.0 ++ ?.(cpu.flags.negative) << 1 in
        let overflow_flag = neg_flag ++ ?.(cpu.flags.overflow) << 1 in
        let reserved_flag = overflow_flag ++ ?.(cpu.flags.reserved) << 1 in
        let break_flag = reserved_flag ++ ?.(cpu.flags.break) << 1 in
        let decimal_flag = break_flag ++ ?.(cpu.flags.decimal) << 1 in
        let zero_flag = decimal_flag ++ ?.(cpu.flags.zero) << 1 in
        zero_flag ++ ?.(cpu.flags.carr_bit)

    let fetch_ui8 (cpu : t) (addr : uint16) : uint8 = RAM.read_ui8 cpu.ram addr
    let fetch_ui16 (cpu : t) (addr : uint16) : uint16 = RAM.read_ui16 cpu.ram addr

    let write_ui8 (cpu : t) (addr : uint16) (value : uint8) =
        RAM.write_ui8 cpu.ram addr value

    let write_ui16 (cpu : t) (addr : uint16) (value : uint16) =
        RAM.write_ui16 cpu.ram addr value

    let absolute_loc_stack (cpu : t) : uint16 = ~^0x0100 +++ !^(cpu.stack_pointer)

    let push_stack_u8 (cpu : t) (value : uint8) : t =
        let stack_loc = absolute_loc_stack cpu in
        write_ui8 cpu stack_loc value;
        { cpu with stack_pointer = cpu.stack_pointer -- ~.0x0001 }

    let push_stack_u16 (cpu : t) (value : uint16) : t =
        let stack_loc = absolute_loc_stack cpu in
        write_ui16 cpu stack_loc value;
        { cpu with stack_pointer = cpu.stack_pointer -- ~.0x0002 }

    let peek_stack (cpu : t) : uint8 =
        let stack_loc = absolute_loc_stack cpu in
        fetch_ui8 cpu stack_loc

    let pop_stack (cpu : t) : t =
        { cpu with stack_pointer = cpu.stack_pointer ++ ~.0x0001 }
end
