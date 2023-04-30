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

    let def_flag () : cpu_flags =
        {
            carr_bit = false;
            zero = false;
            interrupt = true;
            decimal = false;
            negative = false;
            overflow = false;
            break = false;
            reserved = true;
        }

    let nes_cpu (init_pc : uint16) (nes_ram : RAM.t) : t =
        {
            accumulator = ~.0;
            register_X = ~.0;
            register_Y = ~.0;
            program_counter = init_pc;
            stack_pointer = ~.0xFF;
            ram = nes_ram;
            flags = def_flag ();
        }

    let flags_ui8 (cpu : t) : uint8 =
        let neg_flag = ~.0 ++ ?.(cpu.flags.negative) << 1 in
        let overflow_flag = neg_flag ++ ?.(cpu.flags.overflow) << 1 in
        let reserved_flag = overflow_flag ++ ?.(cpu.flags.reserved) << 1 in
        let break_flag = reserved_flag ++ ?.(cpu.flags.break) << 1 in
        let decimal_flag = break_flag ++ ?.(cpu.flags.decimal) << 1 in
        let interrupt_flag = decimal_flag ++ ?.(cpu.flags.interrupt) << 1 in
        let zero_flag = interrupt_flag ++ ?.(cpu.flags.zero) << 1 in
        zero_flag ++ ?.(cpu.flags.carr_bit)

    let flags_from_ui8 (cpu : t) (f : uint8) : t =
        {
            cpu with
            flags =
                {
                    negative = not (f &&. ~.0b10000000 <-> ~.0x00);
                    overflow = not (f &&. ~.0b01000000 <-> ~.0x00);
                    reserved = not (f &&. ~.0b00100000 <-> ~.0x00);
                    break = not (f &&. ~.0b00010000 <-> ~.0x00);
                    decimal = not (f &&. ~.0b00001000 <-> ~.0x00);
                    interrupt = not (f &&. ~.0b00000100 <-> ~.0x00);
                    zero = not (f &&. ~.0b00000010 <-> ~.0x00);
                    carr_bit = not (f &&. ~.0b00000001 <-> ~.0x00);
                };
        }

    let spec_cpu (pc : uint16) (sp : uint8) (acc : uint8) (reg_x : uint8)
            (reg_y : uint8) (flags : uint8) : t =
        let cpu : t =
            {
                accumulator = acc;
                register_X = reg_x;
                register_Y = reg_y;
                program_counter = pc;
                stack_pointer = sp;
                ram = RAM.nes_zero_ram ();
                flags = def_flag ();
            }
        in
        flags_from_ui8 cpu flags

    let fetch_ui8 (cpu : t) (address : uint16) : uint8 =
        RAM.read_ui8 cpu.ram address

    let fetch_ui16 (cpu : t) (address : uint16) : uint16 =
        let lo = fetch_ui8 cpu address in
        let hi = fetch_ui8 cpu (address +++ ~^0x0001) in
        UInt16.combine_ui8 hi lo

    let fetch_current_instruction (cpu : t) : uint8 =
        fetch_ui8 cpu cpu.program_counter

    let write_ui8 (cpu : t) (addr : uint16) (value : uint8) : unit =
        RAM.write_ui8 cpu.ram addr value

    let absolute_loc_stack (cpu : t) : uint16 = ~^0x0100 +++ !^(cpu.stack_pointer)

    let push_stack_ui8 (cpu : t) (value : uint8) : t =
        let stack_loc = absolute_loc_stack cpu in
        write_ui8 cpu stack_loc value;
        { cpu with stack_pointer = cpu.stack_pointer -- ~.0x0001 }

    let push_stack_ui16 (cpu : t) (value : uint16) : t =
        let hi, lo = UInt16.split_ui16 value in
        let pushed_cpu = push_stack_ui8 cpu hi in
        push_stack_ui8 pushed_cpu lo

    let pop_stack_ui8 (cpu : t) : t =
        { cpu with stack_pointer = cpu.stack_pointer ++ ~.0x0001 }

    let pop_stack_ui16 (cpu : t) : t =
        let pop_cpu = pop_stack_ui8 cpu in
        pop_stack_ui8 pop_cpu

    let peek_stack_ui8 (cpu : t) : uint8 =
        let stack_loc = absolute_loc_stack cpu in
        fetch_ui8 cpu (stack_loc +++ ~^0x0001)

    let peek_stack_ui16 (cpu : t) : uint16 =
        let lo = peek_stack_ui8 cpu in
        let pop_cpu = pop_stack_ui8 cpu in
        let hi = peek_stack_ui8 pop_cpu in
        UInt16.combine_ui8 hi lo
end
