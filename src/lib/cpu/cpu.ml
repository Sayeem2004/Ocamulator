open Ram
open UInt8
open UInt16

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

    let nes_cpu (init_pc : uint16) (nes_ram : RAM.t) : t =
        {
            accumulator = ~.0;
            register_X = ~.0;
            register_Y = ~.0;
            program_counter = init_pc;
            stack_pointer = ~.0xFF;
            ram = nes_ram;
            flags =
                {
                    carr_bit = false;
                    zero = false;
                    interrupt = true;
                    decimal = false;
                    negative = false;
                    overflow = false;
                    break = false;
                    reserved = true;
                };
        }

    let flags_ui8 (cpu : t) : uint8 =
        let neg_flag = ~.0 ++ ?.(cpu.flags.negative) << 1 in
        let overflow_flag = neg_flag ++ ?.(cpu.flags.overflow) << 1 in
        let reserved_flag = overflow_flag ++ ?.(cpu.flags.reserved) << 1 in
        let break_flag = reserved_flag ++ ?.(cpu.flags.break) << 1 in
        let decimal_flag = break_flag ++ ?.(cpu.flags.decimal) << 1 in
        let zero_flag = decimal_flag ++ ?.(cpu.flags.zero) << 1 in
        zero_flag ++ ?.(cpu.flags.carr_bit)

    let flags_from_ui8 (cpu : t) (f : uint8) : t =
        {
            cpu with
            flags =
                {
                    cpu.flags with
                    negative = not (f &&. ~.0b1000000 <-> ~.0x00);
                    overflow = not (f &&. ~.0b0100000 <-> ~.0x00);
                    reserved = true;
                    break = not (f &&. ~.0b0001000 <-> ~.0x00);
                    decimal = not (f &&. ~.0b0000100 <-> ~.0x00);
                    zero = not (f &&. ~.0b0000010 <-> ~.0x00);
                    carr_bit = not (f &&. ~.0b0000001 <-> ~.0x00);
                };
        }

    let fetch_ui8 (cpu : t) (address : uint16) : uint8 =
        RAM.read_ui8 cpu.ram address

    let fetch_ui16 (cpu : t) (address : uint16) : uint16 =
        RAM.read_ui16 cpu.ram address

    let fetch_current_instruction (cpu : t) : uint8 =
        fetch_ui8 cpu cpu.program_counter

    let write_ui8 (cpu : t) (addr : uint16) (value : uint8) : unit =
        RAM.write_ui8 cpu.ram addr value

    let write_ui16 (cpu : t) (addr : uint16) (value : uint16) : unit =
        RAM.write_ui16 cpu.ram addr value

    let absolute_loc_stack (cpu : t) : uint16 = ~^0x0100 +++ !^(cpu.stack_pointer)

    let push_stack_ui8 (cpu : t) (value : uint8) : t =
        let stack_loc = absolute_loc_stack cpu in
        write_ui8 cpu stack_loc value;
        { cpu with stack_pointer = cpu.stack_pointer -- ~.0x0001 }

    let push_stack_ui16 (cpu : t) (value : uint16) : t =
        let stack_loc = absolute_loc_stack cpu in
        write_ui16 cpu stack_loc value;
        { cpu with stack_pointer = cpu.stack_pointer -- ~.0x0002 }

    let peek_stack_ui8 (cpu : t) : uint8 =
        let stack_loc = absolute_loc_stack cpu in
        fetch_ui8 cpu (stack_loc +++ ~^0x0001)
    
    let peek_stack_ui16 (cpu : t) : uint16 =
        let stack_loc = absolute_loc_stack cpu in
        fetch_ui16 cpu (stack_loc +++ ~^0x0002)

    let pop_stack_ui8 (cpu : t) : t =
        { cpu with stack_pointer = cpu.stack_pointer ++ ~.0x0001 }

    let pop_stack_ui16 (cpu : t) : t =
        { cpu with stack_pointer = cpu.stack_pointer ++ ~.0x0002 }
end