open Ram
open UInt8
open UInt16

module CPU = struct
    type cpu_flags = {
        negative : bool;
        overflow : bool;
        reserved : bool;
        break : bool;
        decimal : bool;
        interrupt : bool;
        zero : bool;
        carry : bool;
    }

    type t = {
        accumulator : uint8;
        registerX : uint8;
        registerY : uint8;
        progCounter : uint16;
        stackPointer : uint8;
        flags : cpu_flags;
        ram : RAM.t;
    }

    let flags_to_ui8 (flags : cpu_flags) : uint8 =
        let neg_flag = ~.0 ++ ?.(flags.negative) << 1 in
        let overflow_flag = neg_flag ++ ?.(flags.overflow) << 1 in
        let reserved_flag = overflow_flag ++ ?.(flags.reserved) << 1 in
        let break_flag = reserved_flag ++ ?.(flags.break) << 1 in
        let decimal_flag = break_flag ++ ?.(flags.decimal) << 1 in
        let interrupt_flag = decimal_flag ++ ?.(flags.interrupt) << 1 in
        let zero_flag = interrupt_flag ++ ?.(flags.zero) << 1 in
        zero_flag ++ ?.(flags.carry)

    let flags_from_ui8 (flags : uint8) : cpu_flags =
        {
            negative = not (flags &&. ~.0b10000000 <-> ~.0x00);
            overflow = not (flags &&. ~.0b01000000 <-> ~.0x00);
            reserved = not (flags &&. ~.0b00100000 <-> ~.0x00);
            break = not (flags &&. ~.0b00010000 <-> ~.0x00);
            decimal = not (flags &&. ~.0b00001000 <-> ~.0x00);
            interrupt = not (flags &&. ~.0b00000100 <-> ~.0x00);
            zero = not (flags &&. ~.0b00000010 <-> ~.0x00);
            carry = not (flags &&. ~.0b00000001 <-> ~.0x00);
        }

    let nes_cpu (init_pc : uint16) (nes_ram : RAM.t) : t =
        {
            accumulator = ~.0;
            registerX = ~.0;
            registerY = ~.0;
            progCounter = init_pc;
            stackPointer = ~.0xFF;
            ram = nes_ram;
            flags =
                {
                    carry = false;
                    zero = false;
                    interrupt = true;
                    decimal = false;
                    negative = false;
                    overflow = false;
                    break = false;
                    reserved = true;
                };
        }

    let spec_cpu (pc : uint16) (sp : uint8) (acc : uint8) (regX : uint8)
            (regY : uint8) (flags : uint8) (ram : RAM.t) : t =
        {
            accumulator = acc;
            registerX = regX;
            registerY = regY;
            progCounter = pc;
            stackPointer = sp;
            ram;
            flags = flags_from_ui8 flags;
        }

    let fetch_ui8 (cpu : t) (address : uint16) : uint8 =
        RAM.read_ui8 cpu.ram address

    let fetch_ui16 (cpu : t) (address : uint16) : uint16 =
        let lo = fetch_ui8 cpu address in
        let hi = fetch_ui8 cpu (address +++ ~^0x0001) in
        UInt16.combine_ui8 hi lo

    let fetch_instruction (cpu : t) : uint8 = fetch_ui8 cpu cpu.progCounter

    let write_ui8 (cpu : t) (addr : uint16) (value : uint8) : unit =
        RAM.write_ui8 cpu.ram addr value

    let absolute_stack (cpu : t) : uint16 = ~^0x0100 +++ !^(cpu.stackPointer)

    let push_stack_ui8 (cpu : t) (value : uint8) : t =
        let stack_loc = absolute_stack cpu in
        write_ui8 cpu stack_loc value;
        { cpu with stackPointer = cpu.stackPointer -- ~.0x0001 }

    let push_stack_ui16 (cpu : t) (value : uint16) : t =
        let hi, lo = UInt16.split_ui16 value in
        let pushed_cpu = push_stack_ui8 cpu hi in
        push_stack_ui8 pushed_cpu lo

    let pop_stack_ui8 (cpu : t) : t =
        { cpu with stackPointer = cpu.stackPointer ++ ~.0x0001 }

    let pop_stack_ui16 (cpu : t) : t =
        let pop_cpu = pop_stack_ui8 cpu in
        pop_stack_ui8 pop_cpu

    let peek_stack_ui8 (cpu : t) : uint8 =
        let stack_loc = absolute_stack cpu in
        fetch_ui8 cpu (stack_loc +++ ~^0x0001)

    let peek_stack_ui16 (cpu : t) : uint16 =
        let lo = peek_stack_ui8 cpu in
        let pop_cpu = pop_stack_ui8 cpu in
        let hi = peek_stack_ui8 pop_cpu in
        UInt16.combine_ui8 hi lo
end
