open Alias
open Bus

module Cpu (Bus : Bus.CpuBus) = struct
    type t = {
        accumulator : uint8;
        registerX : uint8;
        registerY : uint8;
        progCounter : uint16;
        stackPointer : uint8;
        flags : Flags.t;
        bus : Bus.t;
        data_ui8 : uint8;
        data_ui16 : uint16;
    }

    let spec_cpu (pc : uint16) (sp : uint8) (acc : uint8) (reg_X : uint8)
            (reg_Y : uint8) (flags : uint8) (bus : Bus.t) : t =
        {
            accumulator = acc;
            registerX = reg_X;
            registerY = reg_Y;
            progCounter = pc;
            stackPointer = sp;
            bus;
            flags = Flags.fresh flags;
            data_ui8 = ~.0;
            data_ui16 = ~..0;
        }

    let fetch_ui8 (cpu : t) (addr : uint16) : t =
        let new_bus = Bus.read_ui8 cpu.bus addr in
        {cpu with bus = new_bus; data_ui8 = Bus.data new_bus }

    let fetch_ui16 (cpu : t) (addr : uint16) : t =
        let low_cpu = fetch_ui8 cpu addr in
        let hi_cpu = fetch_ui8 cpu (addr +++ ~..1) in
        { hi_cpu with data_ui16 = !..(hi_cpu.data_ui8) low_cpu.data_ui8 }

    let fetch_op_ui8 (cpu : t) : t = fetch_ui8 cpu cpu.progCounter
    let fetch_op_ui16 (cpu : t) : t = fetch_ui16 cpu cpu.progCounter

    let increment_pc (cpu : t) (inc : int) : t =
        { cpu with progCounter = cpu.progCounter +++ ~..inc }

    let write_ui8 (cpu : t) (addr : uint16) (value : uint8) : t =
        {cpu with bus = Bus.write_ui8 cpu.bus addr value }

    let absolute_stack (cpu : t) : uint16 = ~..0x0100 +++ !**(cpu.stackPointer)

    let push_stack_ui8 (cpu : t) (value : uint8) : t =
        let stack_loc = absolute_stack cpu in
        let pushed_cpu = write_ui8 cpu stack_loc value in
        { pushed_cpu with stackPointer = cpu.stackPointer -- ~.0x01 }

    let push_stack_ui16 (cpu : t) (value : uint16) : t =
        let hi, lo = !@@value in
        let pushed_cpu = push_stack_ui8 cpu hi in
        push_stack_ui8 pushed_cpu lo

    let pop_stack_ui8 (cpu : t) : t =
        { cpu with stackPointer = cpu.stackPointer ++ ~.0x01 }

    let pop_stack_ui16 (cpu : t) : t =
        let pop_cpu = pop_stack_ui8 cpu in
        pop_stack_ui8 pop_cpu

    let peek_stack_ui8 (cpu : t) : t =
        let stack_loc = absolute_stack cpu in
        fetch_ui8 cpu (stack_loc +++ ~..0x01)

    let peek_stack_ui16 (cpu : t) : t =
        let low_cpu = peek_stack_ui8 cpu in
        let pop_cpu = pop_stack_ui8 low_cpu in
        let hi_cpu = peek_stack_ui8 pop_cpu in
        { hi_cpu with data_ui16 = !.. (low_cpu.data_ui8) (hi_cpu.data_ui8) }

    let nmi_vector_addr = ~..0xFFFA
    let reset_vector_addr = ~..0xFFFC
    let irq_vector_addr = ~..0xFFFE

    (** Some people set accumulator and registers X,Y to 0,
        but online it said not to do that or change flags. *)

    let reset (cpu : t) : t =
        let low_cpu = fetch_ui8 cpu reset_vector_addr in
        let hi_cpu = fetch_ui8 low_cpu (reset_vector_addr +++ ~..1) in
        {
            hi_cpu with
            progCounter = !..(hi_cpu.data) low_cpu.data;
            stackPointer = cpu.stackPointer -- ~.0x03;
            flags = Flags.set_interrupt_disable true hi_cpu.flags;
            bus = Bus.reset_interrupt cpu.bus;
        }

    let irq (cpu : t) : t =
        if not (Flags.interrupt_disable cpu.flags) then
            let cpu_pc = push_stack_ui16 cpu cpu.progCounter in
            let cpu_flags =
                {
                    cpu_pc with
                    flags = Flags.set_break false cpu_pc.flags |> Flags.set_reserved true |> Flags.set_interrupt_disable true
                }
            in
            let pushed_flags_cpu = push_stack_ui8 cpu_pc (Flags.status cpu_flags.flags) in
            let low_vector_cpu = fetch_ui8 pushed_flags_cpu irq_vector_addr in
            let hi_vector_cpu = fetch_ui8 low_vector_cpu (irq_vector_addr +++ ~..1) in
            {
                hi_vector_cpu with
                progCounter = !..(hi_vector_cpu.data) low_vector_cpu.data;
                bus = Bus.reset_interrupt pushed_flags_cpu.bus;
            }
        else cpu

    let nmi (cpu : t) : t =
        let pushed_pc_cpu = push_stack_ui16 cpu cpu.progCounter in
        let cpu_with_correct_flags =
            {
                pushed_pc_cpu with
                flags = Flags.set_break false pushed_pc_cpu.flags |> Flags.set_reserved true |> Flags.set_interrupt_disable true
            }
        in
        let pushed_flags_cpu = push_stack_ui8 cpu_with_correct_flags (Flags.status cpu_with_correct_flags.flags) in
        let low_vector_cpu = fetch_ui8 pushed_flags_cpu irq_vector_addr in
        let hi_vector_cpu = fetch_ui8 low_vector_cpu (irq_vector_addr +++ ~..1) in
        {
            hi_vector_cpu with
            progCounter = !..(hi_vector_cpu.data) low_vector_cpu.data;
            bus = Bus.reset_interrupt pushed_flags_cpu.bus;
        }

    let to_string (cpu : t) : string =
        "Cpu {\n" ^ "\tAccumulator: "
        ^ UInt8.to_string cpu.accumulator
        ^ "\n" ^ "\tRegister X: "
        ^ UInt8.to_string cpu.registerX
        ^ "\n" ^ "\tRegister Y: "
        ^ UInt8.to_string cpu.registerY
        ^ "\n" ^ "\tProgram Counter: "
        ^ UInt16.to_string cpu.progCounter
        ^ "\n" ^ "\tStack Pointer: "
        ^ UInt8.to_string cpu.stackPointer
        ^ "\n" ^ "\tFlags: "
        ^ UInt8.to_string (Flags.status cpu.flags)
        ^ "\n" ^ "\tData: "
        ^ UInt8.to_string cpu.data
        ^ "\n" ^ "}"

    (** TODO: Clean this shithole mess up *)

    module Mode = struct
        type _ memory_mode =
            | Accumulator : uint8 memory_mode
            | Absolute : uint16 -> uint16 memory_mode
            | AbsoluteX : uint16 -> uint16 memory_mode
            | AbsoluteY : uint16 -> uint16 memory_mode
            | Immediate : uint8 -> uint8 memory_mode
            | Indirect : uint16 -> uint16 memory_mode
            | XIndirect : uint8 -> uint16 memory_mode
            | IndirectY : uint8 -> uint16 memory_mode
            | Relative : uint8 -> uint16 memory_mode
            | Zeropage : uint8 -> uint8 memory_mode
            | ZeropageX : uint8 -> uint16 memory_mode
            | ZeropageY : uint8 -> uint16 memory_mode

        let contents (cpu : t) (type a) (mode : a memory_mode) : uint8 =
            match mode with
            | Accumulator -> cpu.accumulator
            | Absolute addr -> fetch_ui8 cpu addr
            | AbsoluteX addr ->
                let addr = addr +++ !**(cpu.registerX) in
                fetch_ui8 cpu addr
            | AbsoluteY addr ->
                let addr = addr +++ !**(cpu.registerY) in
                fetch_ui8 cpu addr
            | Immediate addr -> addr
            | Indirect addr ->
                let addr = fetch_ui16 cpu addr in
                fetch_ui8 cpu addr
            | XIndirect addr ->
                let low = fetch_ui8 cpu !**(addr ++ cpu.registerX) in
                let hig = fetch_ui8 cpu !**(addr ++ cpu.registerX ++ ~.0x01) in
                let addr = !..hig low in
                fetch_ui8 cpu addr
            | IndirectY addr ->
                let low = fetch_ui8 cpu !**addr in
                let hig = fetch_ui8 cpu !**(addr ++ ~.0x01) in
                let addr = !..hig low +++ !**(cpu.registerY) in
                fetch_ui8 cpu addr
            | Relative addr ->
                let addr = !**addr +++ cpu.progCounter in
                fetch_ui8 cpu addr
            | Zeropage addr ->
                let addr = !**addr in
                fetch_ui8 cpu addr
            | ZeropageX addr ->
                let addr = !**(addr ++ cpu.registerX) in
                fetch_ui8 cpu addr
            | ZeropageY addr ->
                let addr = !**(addr ++ cpu.registerY) in
                fetch_ui8 cpu addr

        let address (cpu : t) (type a) (mode : a memory_mode) : uint16 =
            match mode with
            | Absolute addr -> addr
            | AbsoluteX addr -> addr +++ !**(cpu.registerX)
            | AbsoluteY addr -> addr +++ !**(cpu.registerY)
            | Indirect addr -> fetch_ui16 cpu addr
            | XIndirect addr ->
                let low = fetch_ui8 cpu !**(addr ++ cpu.registerX) in
                let hig = fetch_ui8 cpu !**(addr ++ cpu.registerX ++ ~.0x01) in
                !..hig low
            | IndirectY addr ->
                let low = fetch_ui8 cpu !**addr in
                let hig = fetch_ui8 cpu !**(addr ++ ~.0x01) in
                !..hig low +++ !**(cpu.registerY)
            | Relative addr -> ~..(?%addr + ~**(cpu.progCounter))
            | Zeropage addr -> !**addr
            | ZeropageX addr -> !**(addr ++ cpu.registerX)
            | ZeropageY addr -> !**(addr ++ cpu.registerY)
            | _ -> raise InvalidValue
    end

    open Mode

    module Instruction = struct
        let adc_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let sum = ~*(cpu.accumulator) + ~*contents + ~*(?.(cpu.flags.carry)) in
            let res, acc = (~.sum, cpu.accumulator) in
            let carr, zero, ngtv = (sum > 0xFF, ?*res, ?-res) in
            let over = ?@(acc |&. contents) &&. (acc |&. res) &&. ~.0x80 > ~.0x00 in
            {
                cpu with
                accumulator = res;
                flags =
                    {
                        cpu.flags with
                        carry = carr;
                        zero;
                        overflow = over;
                        negative = ngtv;
                    };
            }

        let and_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let operand = contents cpu mode in
            let modif_acc = cpu.accumulator &&. operand in
            let zero, ngtv = (?*modif_acc, ?-modif_acc) in
            {
                cpu with
                accumulator = modif_acc;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let asl_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            match mode with
            | Accumulator ->
                let acc = cpu.accumulator << 1 in
                let carr, zero, ngtv = (?-(cpu.accumulator), ?*acc, ?-acc) in
                {
                    cpu with
                    accumulator = acc;
                    flags = { cpu.flags with carry = carr; zero; negative = ngtv };
                }
            | addr_mode ->
                let address = address cpu addr_mode in
                let contents = contents cpu addr_mode in
                let shifted = contents << 1 in
                let carr, zero, ngtv = (?-contents, ?*shifted, ?-shifted) in
                write_ui8 cpu address shifted;
                {
                    cpu with
                    flags = { cpu.flags with carry = carr; zero; negative = ngtv };
                }

        let bcc_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            if not cpu.flags.carry then { cpu with progCounter = address cpu mode }
            else cpu

        let bcs_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            if cpu.flags.carry then { cpu with progCounter = address cpu mode }
            else cpu

        let beq_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            if cpu.flags.zero then { cpu with progCounter = address cpu mode }
            else cpu

        let bit_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let mem_contents = contents cpu mode in
            let zero = ?*(mem_contents &&. cpu.accumulator) in
            let bit_6, bit_7 = (?-(mem_contents << 1), ?-mem_contents) in
            {
                cpu with
                flags = { cpu.flags with zero; overflow = bit_6; negative = bit_7 };
            }

        let bmi_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            if cpu.flags.negative then { cpu with progCounter = address cpu mode }
            else cpu

        let bne_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            if not cpu.flags.zero then { cpu with progCounter = address cpu mode }
            else cpu

        let bpl_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            if not cpu.flags.negative then { cpu with progCounter = address cpu mode }
            else cpu

        let brk_op (cpu : t) : t =
            let cpu_pc = push_stack_ui16 cpu (cpu.progCounter +++ ~..1) in
            let flags = flags_to_ui8 { cpu_pc.flags with break = true } in
            let cpu_flags = push_stack_ui8 cpu_pc flags in
            let vector = fetch_ui16 cpu_flags irq_vector_addr in
            {
                cpu_flags with
                progCounter = vector;
                flags = { cpu_flags.flags with interrupt_disable = true };
            }

        let bvc_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            if not cpu.flags.overflow then { cpu with progCounter = address cpu mode }
            else cpu

        let bvs_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            if cpu.flags.overflow then { cpu with progCounter = address cpu mode }
            else cpu

        let clc_op (cpu : t) : t =
            { cpu with flags = { cpu.flags with carry = false } }

        let cld_op (cpu : t) : t =
            { cpu with flags = { cpu.flags with decimal = false } }

        let cli_op (cpu : t) : t =
            { cpu with flags = { cpu.flags with interrupt_disable = false } }

        let clv_op (cpu : t) : t =
            { cpu with flags = { cpu.flags with overflow = false } }

        let cmp_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let sub_acc = cpu.accumulator -- contents in
            let carr = cpu.accumulator <?> contents >= 0 in
            let zero = cpu.accumulator == contents in
            let ngtv = ?-sub_acc in
            {
                cpu with
                flags = { cpu.flags with carry = carr; zero; negative = ngtv };
            }

        let cpx_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let subX = cpu.registerX -- contents in
            let carr = cpu.registerX <?> contents >= 0 in
            let zero = cpu.registerX == contents in
            let ngtv = ?-subX in
            {
                cpu with
                flags = { cpu.flags with carry = carr; zero; negative = ngtv };
            }

        let cpy_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let subY = cpu.registerY -- contents in
            let carr = cpu.registerY <?> contents >= 0 in
            let zero = cpu.registerY == contents in
            let ngtv = ?-subY in
            {
                cpu with
                flags = { cpu.flags with carry = carr; zero; negative = ngtv };
            }

        let dec_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let address = address cpu mode in
            let decrement = contents -- ~.0x01 in
            let zero, ngtv = (?*decrement, ?-decrement) in
            write_ui8 cpu address decrement;
            { cpu with flags = { cpu.flags with zero; negative = ngtv } }

        let dex_op (cpu : t) : t =
            let decrement = cpu.registerX -- ~.0x01 in
            let zero, ngtv = (?*decrement, ?-decrement) in
            {
                cpu with
                registerX = decrement;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let dey_op (cpu : t) : t =
            let decrement = cpu.registerY -- ~.0x01 in
            let zero, ngtv = (?*decrement, ?-decrement) in
            {
                cpu with
                registerY = decrement;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let eor_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let xor_acc = cpu.accumulator |&. contents in
            let zero, ngtv = (?*xor_acc, ?-xor_acc) in
            {
                cpu with
                accumulator = xor_acc;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let inc_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let address = address cpu mode in
            let increment = contents ++ ~.0x01 in
            let zero, ngtv = (?*increment, ?-increment) in
            write_ui8 cpu address increment;
            { cpu with flags = { cpu.flags with zero; negative = ngtv } }

        let inx_op (cpu : t) : t =
            let increment = cpu.registerX ++ ~.0x01 in
            let zero, ngtv = (?*increment, ?-increment) in
            {
                cpu with
                registerX = increment;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let iny_op (cpu : t) : t =
            let increment = cpu.registerY ++ ~.0x01 in
            let zero, ngtv = (?*increment, ?-increment) in
            {
                cpu with
                registerY = increment;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let jmp_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            { cpu with progCounter = address cpu mode }

        let jsr_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let return = cpu.progCounter --- ~..0x01 in
            let pushed = push_stack_ui16 cpu return in
            { pushed with progCounter = address pushed mode }

        let lda_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let zero, ngtv = (?*contents, ?-contents) in
            {
                cpu with
                accumulator = contents;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let ldx_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let zero, ngtv = (?*contents, ?-contents) in
            {
                cpu with
                registerX = contents;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let ldy_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let zero, ngtv = (?*contents, ?-contents) in
            {
                cpu with
                registerY = contents;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let lsr_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            match mode with
            | Accumulator ->
                let shifted = cpu.accumulator >> 1 in
                let carr, zero, ngtv = (?+(cpu.accumulator), ?*shifted, ?-shifted) in
                {
                    cpu with
                    accumulator = shifted;
                    flags = { cpu.flags with carry = carr; zero; negative = ngtv };
                }
            | addr_mode ->
                let address = address cpu addr_mode in
                let contents = contents cpu addr_mode in
                let shifted = contents >> 1 in
                let carr, zero, ngtv = (?+contents, ?*shifted, ?-shifted) in
                write_ui8 cpu address shifted;
                {
                    cpu with
                    flags = { cpu.flags with carry = carr; zero; negative = ngtv };
                }

        let nop_op (cpu : t) : t = cpu

        let ora_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let or_acc = cpu.accumulator ||. contents in
            let zero, ngtv = (?*or_acc, ?-or_acc) in
            {
                cpu with
                accumulator = or_acc;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let pha_op (cpu : t) : t = push_stack_ui8 cpu cpu.accumulator

        let php_op (cpu : t) : t =
            let flags = { cpu.flags with break = true; reserved = true } in
            flags_to_ui8 flags |> push_stack_ui8 cpu

        let pla_op (cpu : t) : t =
            let pulled = peek_stack_ui8 cpu in
            let popped = pop_stack_ui8 cpu in
            let zero, ngtv = (?*pulled, ?-pulled) in
            {
                popped with
                accumulator = pulled;
                flags = { popped.flags with zero; negative = ngtv };
            }

        let plp_op (cpu : t) : t =
            let pulled = peek_stack_ui8 cpu in
            let popped = pop_stack_ui8 cpu in
            let flags = flags_from_ui8 pulled in
            { popped with flags = { flags with break = false; reserved = true } }

        let rol_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            match mode with
            | Accumulator ->
                let shifted = (cpu.accumulator << 1) ++ ?.(cpu.flags.carry) in
                let carr, zero, ngtv = (?-(cpu.accumulator), ?*shifted, ?-shifted) in
                {
                    cpu with
                    accumulator = shifted;
                    flags = { cpu.flags with carry = carr; zero; negative = ngtv };
                }
            | addr_mode ->
                let address = address cpu addr_mode in
                let contents = contents cpu addr_mode in
                let shifted = (contents << 1) ++ ?.(cpu.flags.carry) in
                let carr, zero, ngtv = (?-contents, ?*shifted, ?-shifted) in
                write_ui8 cpu address shifted;
                {
                    cpu with
                    flags = { cpu.flags with carry = carr; zero; negative = ngtv };
                }

        let ror_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            match mode with
            | Accumulator ->
                let acc = cpu.accumulator in
                let shifted = acc >> 1 &&. ~.0x7F ||. (?.(cpu.flags.carry) << 7) in
                let carr, zero, ngtv = (?+acc, ?*shifted, ?-shifted) in
                {
                    cpu with
                    accumulator = shifted;
                    flags = { cpu.flags with carry = carr; zero; negative = ngtv };
                }
            | addr_mode ->
                let address = address cpu addr_mode in
                let contents = contents cpu addr_mode in
                let shifted =
                    contents >> 1 &&. ~.0x7F ||. (?.(cpu.flags.carry) << 7)
                in
                let carr, zero, ngtv = (?+contents, ?*shifted, ?-shifted) in
                write_ui8 cpu address shifted;
                {
                    cpu with
                    flags = { cpu.flags with carry = carr; zero; negative = ngtv };
                }

        let rti_op (cpu : t) : t =
            let flags = flags_from_ui8 (peek_stack_ui8 cpu) in
            let popped_flags = pop_stack_ui8 cpu in
            let counter = peek_stack_ui16 popped_flags in
            let popped_pc = pop_stack_ui16 popped_flags in
            {
                popped_pc with
                progCounter = counter;
                flags = { flags with break = false; reserved = true };
            }

        let rts_op (cpu : t) : t =
            let counter = peek_stack_ui16 cpu +++ ~..0x01 in
            let popped_pc = pop_stack_ui16 cpu in
            { popped_pc with progCounter = counter }

        let sbc_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let sum =
                ~*(cpu.accumulator) + ~*(?@contents) + ~*(?.(cpu.flags.carry))
            in
            let res, acc = (~.sum, cpu.accumulator) in
            let carr, zero, ngtv = (sum > 0xFF, ?*res, ?-res) in
            let over = acc |&. contents &&. (acc |&. res) &&. ~.0x80 > ~.0x00 in
            {
                cpu with
                accumulator = res;
                flags =
                    {
                        cpu.flags with
                        carry = carr;
                        zero;
                        overflow = over;
                        negative = ngtv;
                    };
            }

        let sec_op (cpu : t) : t =
            { cpu with flags = { cpu.flags with carry = true } }

        let sed_op (cpu : t) : t =
            { cpu with flags = { cpu.flags with decimal = true } }

        let sei_op (cpu : t) : t =
            { cpu with flags = { cpu.flags with interrupt_disable = true } }

        let sta_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let address = address cpu mode in
            write_ui8 cpu address cpu.accumulator;
            cpu

        let stx_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let address = address cpu mode in
            write_ui8 cpu address cpu.registerX;
            cpu

        let sty_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let address = address cpu mode in
            write_ui8 cpu address cpu.registerY;
            cpu

        let tax_op (cpu : t) : t =
            let zero, ngtv = (?*(cpu.accumulator), ?-(cpu.accumulator)) in
            {
                cpu with
                registerX = cpu.accumulator;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let tay_op (cpu : t) : t =
            let zero, ngtv = (?*(cpu.accumulator), ?-(cpu.accumulator)) in
            {
                cpu with
                registerY = cpu.accumulator;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let tsx_op (cpu : t) : t =
            let zero, ngtv = (?*(cpu.stackPointer), ?-(cpu.stackPointer)) in
            {
                cpu with
                registerX = cpu.stackPointer;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let txa_op (cpu : t) : t =
            let zero, ngtv = (?*(cpu.registerX), ?-(cpu.registerX)) in
            {
                cpu with
                accumulator = cpu.registerX;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let txs_op (cpu : t) : t = { cpu with stackPointer = cpu.registerX }

        let tya_op (cpu : t) : t =
            let zero, ngtv = (?*(cpu.registerY), ?-(cpu.registerY)) in
            {
                cpu with
                accumulator = cpu.registerY;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let top_op (type a') (mode : a' memory_mode) (cpu : t) : t = cpu

        let alr_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            and_op mode cpu |> lsr_op Accumulator

        let dop_op (type a') (mode : a' memory_mode) (cpu : t) : t = cpu

        let anc_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let and_acc = cpu.accumulator &&. contents in
            let zero, ngtv = (?*and_acc, ?-and_acc) in
            {
                cpu with
                accumulator = and_acc;
                flags = { cpu.flags with zero; negative = ngtv; carry = ngtv };
            }

        let sax_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let and_acc = cpu.accumulator &&. cpu.registerX in
            write_ui8 cpu (address cpu mode) and_acc;
            cpu

        let ane_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let modif_acc =
                ~.0xEE ||. cpu.accumulator &&. cpu.registerX &&. contents
            in
            let zero, ngtv = (?*modif_acc, ?-modif_acc) in
            {
                cpu with
                accumulator = modif_acc;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let lax_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            lda_op mode cpu |> ldx_op mode

        let las_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let value = cpu.stackPointer &&. contents in
            let zero, ngtv = (?*value, ?-value) in
            {
                cpu with
                accumulator = value;
                registerX = value;
                stackPointer = value;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let dcp_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            dec_op mode cpu |> cmp_op mode

        let lxa_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let contents = contents cpu mode in
            let modif_acc = ~.0xEE ||. cpu.accumulator &&. contents in
            let zero, ngtv = (?*modif_acc, ?-modif_acc) in
            {
                cpu with
                accumulator = modif_acc;
                registerX = modif_acc;
                flags = { cpu.flags with zero; negative = ngtv };
            }

        let isc_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            inc_op mode cpu |> sbc_op mode

        let rla_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            rol_op mode cpu |> and_op mode

        let slo_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            asl_op mode cpu |> ora_op mode

        let sre_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            lsr_op mode cpu |> eor_op mode

        let jam_op (cpu : t) : t =
            { cpu with progCounter = cpu.progCounter --- ~..0x01 }

        let rra_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            ror_op mode cpu |> adc_op mode

        let sbx_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let sub_sec = contents cpu mode in
            let sub_fir = cpu.accumulator &&. cpu.registerX in
            let sub_res = sub_fir -- sub_sec in
            let carr, zero, ngtv = (sub_fir <?> sub_res >= 0, ?*sub_res, ?-sub_res) in
            {
                cpu with
                registerX = sub_res;
                flags = { cpu.flags with carry = carr; zero; negative = ngtv };
            }

        let sha_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let address = address cpu mode in
            let high = !--(address &&& ~..0xFF00 >>> 8) in
            let value = cpu.accumulator &&. cpu.registerX &&. high ++ ~.0x01 in
            write_ui8 cpu address value;
            cpu

        let tas_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let stack_val = cpu.accumulator &&. cpu.registerX in
            let new_cpu = { cpu with stackPointer = stack_val } in
            let address = address new_cpu mode in
            let high = !--(address &&& ~..0xFF00 >>> 8) in
            let value = stack_val &&. high ++ ~.0x01 in
            write_ui8 new_cpu address value;
            new_cpu

        let shy_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let address = address cpu mode in
            let high = !--(address &&& ~..0xFF00 >>> 8) in
            let value = cpu.registerY &&. high ++ ~.0x01 in
            write_ui8 cpu address value;
            cpu

        let shx_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let address = address cpu mode in
            let high = !--(address &&& ~..0xFF00 >>> 8) in
            let value = cpu.registerX &&. high ++ ~.0x01 in
            write_ui8 cpu address value;
            cpu

        let arr_op (type a') (mode : a' memory_mode) (cpu : t) : t =
            let value = cpu.accumulator &&. contents cpu mode >> 1 in
            let value = if cpu.flags.carry then value ||. ~.0x80 else value in
            let carr, zero = (not (value &&. ~.0x01 <-> ~.0), value <-> ~.0) in
            let ngtv = not (value &&. ~.0x80 <-> ~.0) in
            let over = not (value &&. (value << 1) &&. ~.0x40 <-> ~.0) in
            {
                cpu with
                accumulator = value;
                flags =
                    {
                        cpu.flags with
                        carry = carr;
                        zero;
                        negative = ngtv;
                        overflow = over;
                    };
            }
    end

    open Instruction

    module Decode = struct
        type none_inst = t -> t
        type 'a some_inst = 'a memory_mode -> t -> t

        let step_none_inst (cpu : t) (inst : none_inst) : t = inst cpu

        let step_accm_inst (cpu : t) (inst : uint8 some_inst) : t =
            inst Accumulator cpu

        let step_abst_inst (cpu : t) (inst : uint16 some_inst) : t =
            let operand = fetch_ui16_op cpu in
            let step_cpu = increment_pc cpu 2 in
            inst (Absolute operand) step_cpu

        let step_absx_inst (cpu : t) (inst : uint16 some_inst) : t =
            let operand = fetch_ui16_op cpu in
            let step_cpu = increment_pc cpu 2 in
            inst (AbsoluteX operand) step_cpu

        let step_absy_inst (cpu : t) (inst : uint16 some_inst) : t =
            let operand = fetch_ui16_op cpu in
            let step_cpu = increment_pc cpu 2 in
            inst (AbsoluteY operand) step_cpu

        let step_imed_inst (cpu : t) (inst : uint8 some_inst) : t =
            let operand = fetch_ui8_op cpu in
            let step_cpu = increment_pc cpu 1 in
            inst (Immediate operand) step_cpu

        let step_indr_inst (cpu : t) (inst : uint16 some_inst) : t =
            let operand = fetch_ui16_op cpu in
            let step_cpu = increment_pc cpu 2 in
            inst (Indirect operand) step_cpu

        let step_xind_inst (cpu : t) (inst : uint16 some_inst) : t =
            let operand = fetch_ui8_op cpu in
            let step_cpu = increment_pc cpu 1 in
            inst (XIndirect operand) step_cpu

        let step_indy_inst (cpu : t) (inst : uint16 some_inst) : t =
            let operand = fetch_ui8_op cpu in
            let step_cpu = increment_pc cpu 1 in
            inst (IndirectY operand) step_cpu

        let step_relt_inst (cpu : t) (inst : uint16 some_inst) : t =
            let operand = fetch_ui8_op cpu in
            let step_cpu = increment_pc cpu 1 in
            inst (Relative operand) step_cpu

        let step_zero_inst (cpu : t) (inst : uint8 some_inst) : t =
            let operand = fetch_ui8_op cpu in
            let step_cpu = increment_pc cpu 1 in
            inst (Zeropage operand) step_cpu

        let step_zerx_inst (cpu : t) (inst : uint16 some_inst) : t =
            let operand = fetch_ui8_op cpu in
            let step_cpu = increment_pc cpu 1 in
            inst (ZeropageX operand) step_cpu

        let step_zery_inst (cpu : t) (inst : uint16 some_inst) : t =
            let operand = fetch_ui8_op cpu in
            let step_cpu = increment_pc cpu 1 in
            inst (ZeropageY operand) step_cpu

        let step_none (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x00 -> step_none_inst cpu brk_op
            | 0x02 -> step_none_inst cpu jam_op
            | 0x08 -> step_none_inst cpu php_op
            | 0x12 -> step_none_inst cpu jam_op
            | 0x18 -> step_none_inst cpu clc_op
            | 0x1A -> step_none_inst cpu nop_op
            | 0x22 -> step_none_inst cpu jam_op
            | 0x28 -> step_none_inst cpu plp_op
            | 0x32 -> step_none_inst cpu jam_op
            | 0x38 -> step_none_inst cpu sec_op
            | 0x3A -> step_none_inst cpu nop_op
            | 0x40 -> step_none_inst cpu rti_op
            | 0x42 -> step_none_inst cpu jam_op
            | 0x48 -> step_none_inst cpu pha_op
            | 0x52 -> step_none_inst cpu jam_op
            | 0x58 -> step_none_inst cpu cli_op
            | 0x5A -> step_none_inst cpu nop_op
            | 0x60 -> step_none_inst cpu rts_op
            | 0x62 -> step_none_inst cpu jam_op
            | 0x68 -> step_none_inst cpu pla_op
            | 0x72 -> step_none_inst cpu jam_op
            | 0x78 -> step_none_inst cpu sei_op
            | 0x7A -> step_none_inst cpu nop_op
            | 0x88 -> step_none_inst cpu dey_op
            | 0x8A -> step_none_inst cpu txa_op
            | 0x92 -> step_none_inst cpu jam_op
            | 0x98 -> step_none_inst cpu tya_op
            | 0x9A -> step_none_inst cpu txs_op
            | 0xA8 -> step_none_inst cpu tay_op
            | 0xAA -> step_none_inst cpu tax_op
            | 0xB2 -> step_none_inst cpu jam_op
            | 0xB8 -> step_none_inst cpu clv_op
            | 0xBA -> step_none_inst cpu tsx_op
            | 0xC8 -> step_none_inst cpu iny_op
            | 0xCA -> step_none_inst cpu dex_op
            | 0xD2 -> step_none_inst cpu jam_op
            | 0xD8 -> step_none_inst cpu cld_op
            | 0xDA -> step_none_inst cpu nop_op
            | 0xE8 -> step_none_inst cpu inx_op
            | 0xEA -> step_none_inst cpu nop_op
            | 0xF2 -> step_none_inst cpu jam_op
            | 0xF8 -> step_none_inst cpu sed_op
            | 0xFA -> step_none_inst cpu nop_op
            | _ -> nop_op cpu

        let step_accm (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x0A -> step_accm_inst cpu asl_op
            | 0x2A -> step_accm_inst cpu rol_op
            | 0x4A -> step_accm_inst cpu lsr_op
            | 0x6A -> step_accm_inst cpu ror_op
            | _ -> nop_op cpu

        let step_abst (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x0C -> step_abst_inst cpu top_op
            | 0x0D -> step_abst_inst cpu ora_op
            | 0x0E -> step_abst_inst cpu asl_op
            | 0x0F -> step_abst_inst cpu slo_op
            | 0x20 -> step_abst_inst cpu jsr_op
            | 0x2C -> step_abst_inst cpu bit_op
            | 0x2D -> step_abst_inst cpu and_op
            | 0x2E -> step_abst_inst cpu rol_op
            | 0x2F -> step_abst_inst cpu rla_op
            | 0x4C -> step_abst_inst cpu jmp_op
            | 0x4D -> step_abst_inst cpu eor_op
            | 0x4E -> step_abst_inst cpu lsr_op
            | 0x4F -> step_abst_inst cpu sre_op
            | 0x6D -> step_abst_inst cpu adc_op
            | 0x6E -> step_abst_inst cpu ror_op
            | 0x6F -> step_abst_inst cpu rra_op
            | 0x8C -> step_abst_inst cpu sty_op
            | 0x8D -> step_abst_inst cpu sta_op
            | 0x8E -> step_abst_inst cpu stx_op
            | 0x8F -> step_abst_inst cpu sax_op
            | 0xAC -> step_abst_inst cpu ldy_op
            | 0xAD -> step_abst_inst cpu lda_op
            | 0xAE -> step_abst_inst cpu ldx_op
            | 0xAF -> step_abst_inst cpu lax_op
            | 0xCC -> step_abst_inst cpu cpy_op
            | 0xCD -> step_abst_inst cpu cmp_op
            | 0xCE -> step_abst_inst cpu dec_op
            | 0xCF -> step_abst_inst cpu dcp_op
            | 0xEC -> step_abst_inst cpu cpx_op
            | 0xED -> step_abst_inst cpu sbc_op
            | 0xEE -> step_abst_inst cpu inc_op
            | 0xEF -> step_abst_inst cpu isc_op
            | _ -> nop_op cpu

        let step_absx (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x1C -> step_absx_inst cpu top_op
            | 0x1D -> step_absx_inst cpu ora_op
            | 0x1E -> step_absx_inst cpu asl_op
            | 0x1F -> step_absx_inst cpu slo_op
            | 0x3C -> step_absx_inst cpu top_op
            | 0x3D -> step_absx_inst cpu and_op
            | 0x3E -> step_absx_inst cpu rol_op
            | 0x3F -> step_absx_inst cpu rla_op
            | 0x5C -> step_absx_inst cpu top_op
            | 0x5D -> step_absx_inst cpu eor_op
            | 0x5E -> step_absx_inst cpu lsr_op
            | 0x5F -> step_absx_inst cpu sre_op
            | 0x7C -> step_absx_inst cpu top_op
            | 0x7D -> step_absx_inst cpu adc_op
            | 0x7E -> step_absx_inst cpu ror_op
            | 0x7F -> step_absx_inst cpu rra_op
            | 0x9C -> step_absx_inst cpu shy_op
            | 0x9D -> step_absx_inst cpu sta_op
            | 0xBC -> step_absx_inst cpu ldy_op
            | 0xBD -> step_absx_inst cpu lda_op
            | 0xDC -> step_absx_inst cpu top_op
            | 0xDD -> step_absx_inst cpu cmp_op
            | 0xDE -> step_absx_inst cpu dec_op
            | 0xDF -> step_absx_inst cpu dcp_op
            | 0xFC -> step_absx_inst cpu top_op
            | 0xFD -> step_absx_inst cpu sbc_op
            | 0xFE -> step_absx_inst cpu inc_op
            | 0xFF -> step_absx_inst cpu isc_op
            | _ -> nop_op cpu

        let step_absy (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x19 -> step_absy_inst cpu ora_op
            | 0x1B -> step_absy_inst cpu slo_op
            | 0x39 -> step_absy_inst cpu and_op
            | 0x3B -> step_absy_inst cpu rla_op
            | 0x59 -> step_absy_inst cpu eor_op
            | 0x5B -> step_absy_inst cpu sre_op
            | 0x79 -> step_absy_inst cpu adc_op
            | 0x7B -> step_absy_inst cpu rra_op
            | 0x99 -> step_absy_inst cpu sta_op
            | 0x9B -> step_absy_inst cpu tas_op
            | 0x9E -> step_absy_inst cpu shx_op
            | 0x9F -> step_absy_inst cpu sha_op
            | 0xBB -> step_absy_inst cpu las_op
            | 0xB9 -> step_absy_inst cpu lda_op
            | 0xBE -> step_absy_inst cpu ldx_op
            | 0xBF -> step_absy_inst cpu lax_op
            | 0xDB -> step_absy_inst cpu dcp_op
            | 0xD9 -> step_absy_inst cpu cmp_op
            | 0xF9 -> step_absy_inst cpu sbc_op
            | 0xFB -> step_absy_inst cpu isc_op
            | _ -> nop_op cpu

        let step_imed (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x09 -> step_imed_inst cpu ora_op
            | 0x0B -> step_imed_inst cpu anc_op
            | 0x29 -> step_imed_inst cpu and_op
            | 0x2B -> step_imed_inst cpu anc_op
            | 0x49 -> step_imed_inst cpu eor_op
            | 0x4B -> step_imed_inst cpu alr_op
            | 0x69 -> step_imed_inst cpu adc_op
            | 0x6B -> step_imed_inst cpu arr_op
            | 0x80 -> step_imed_inst cpu dop_op
            | 0x82 -> step_imed_inst cpu dop_op
            | 0x89 -> step_imed_inst cpu dop_op
            | 0x8B -> step_imed_inst cpu ane_op
            | 0xA0 -> step_imed_inst cpu ldy_op
            | 0xA2 -> step_imed_inst cpu ldx_op
            | 0xA9 -> step_imed_inst cpu lda_op
            | 0xAB -> step_imed_inst cpu lxa_op
            | 0xC0 -> step_imed_inst cpu cpy_op
            | 0xC2 -> step_imed_inst cpu dop_op
            | 0xC9 -> step_imed_inst cpu cmp_op
            | 0xCB -> step_imed_inst cpu sbx_op
            | 0xE0 -> step_imed_inst cpu cpx_op
            | 0xE2 -> step_imed_inst cpu dop_op
            | 0xE9 -> step_imed_inst cpu sbc_op
            | 0xEB -> step_imed_inst cpu sbc_op
            | _ -> nop_op cpu

        let step_indr (opcode : int) (cpu : t) : t =
            match opcode with 0x6C -> step_indr_inst cpu jmp_op | _ -> nop_op cpu

        let step_xind (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x01 -> step_xind_inst cpu ora_op
            | 0x03 -> step_xind_inst cpu slo_op
            | 0x21 -> step_xind_inst cpu and_op
            | 0x23 -> step_xind_inst cpu rla_op
            | 0x41 -> step_xind_inst cpu eor_op
            | 0x43 -> step_xind_inst cpu sre_op
            | 0x61 -> step_xind_inst cpu adc_op
            | 0x63 -> step_xind_inst cpu rra_op
            | 0x81 -> step_xind_inst cpu sta_op
            | 0x83 -> step_xind_inst cpu sax_op
            | 0xA1 -> step_xind_inst cpu lda_op
            | 0xA3 -> step_xind_inst cpu lax_op
            | 0xC1 -> step_xind_inst cpu cmp_op
            | 0xC3 -> step_xind_inst cpu dcp_op
            | 0xE1 -> step_xind_inst cpu sbc_op
            | 0xE3 -> step_xind_inst cpu isc_op
            | _ -> nop_op cpu

        let step_indy (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x11 -> step_indy_inst cpu ora_op
            | 0x13 -> step_indy_inst cpu slo_op
            | 0x31 -> step_indy_inst cpu and_op
            | 0x33 -> step_indy_inst cpu rla_op
            | 0x51 -> step_indy_inst cpu eor_op
            | 0x53 -> step_indy_inst cpu sre_op
            | 0x71 -> step_indy_inst cpu adc_op
            | 0x73 -> step_indy_inst cpu rra_op
            | 0x91 -> step_indy_inst cpu sta_op
            | 0x93 -> step_indy_inst cpu sha_op
            | 0xB1 -> step_indy_inst cpu lda_op
            | 0XB3 -> step_indy_inst cpu lax_op
            | 0xD1 -> step_indy_inst cpu cmp_op
            | 0xD3 -> step_indy_inst cpu dcp_op
            | 0xF1 -> step_indy_inst cpu sbc_op
            | 0xF3 -> step_indy_inst cpu isc_op
            | _ -> nop_op cpu

        let step_relt (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x10 -> step_relt_inst cpu bpl_op
            | 0x30 -> step_relt_inst cpu bmi_op
            | 0x50 -> step_relt_inst cpu bvc_op
            | 0x70 -> step_relt_inst cpu bvs_op
            | 0x90 -> step_relt_inst cpu bcc_op
            | 0xB0 -> step_relt_inst cpu bcs_op
            | 0xD0 -> step_relt_inst cpu bne_op
            | 0xF0 -> step_relt_inst cpu beq_op
            | _ -> nop_op cpu

        let step_zero (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x04 -> step_zero_inst cpu dop_op
            | 0x05 -> step_zero_inst cpu ora_op
            | 0x06 -> step_zero_inst cpu asl_op
            | 0x07 -> step_zero_inst cpu slo_op
            | 0x24 -> step_zero_inst cpu bit_op
            | 0x25 -> step_zero_inst cpu and_op
            | 0x26 -> step_zero_inst cpu rol_op
            | 0x27 -> step_zero_inst cpu rla_op
            | 0x44 -> step_zero_inst cpu dop_op
            | 0x45 -> step_zero_inst cpu eor_op
            | 0x47 -> step_zero_inst cpu sre_op
            | 0x46 -> step_zero_inst cpu lsr_op
            | 0x64 -> step_zero_inst cpu dop_op
            | 0x65 -> step_zero_inst cpu adc_op
            | 0x66 -> step_zero_inst cpu ror_op
            | 0x67 -> step_zero_inst cpu rra_op
            | 0x84 -> step_zero_inst cpu sty_op
            | 0x85 -> step_zero_inst cpu sta_op
            | 0x86 -> step_zero_inst cpu stx_op
            | 0x87 -> step_zero_inst cpu sax_op
            | 0xA4 -> step_zero_inst cpu ldy_op
            | 0xA5 -> step_zero_inst cpu lda_op
            | 0xA6 -> step_zero_inst cpu ldx_op
            | 0xA7 -> step_zero_inst cpu lax_op
            | 0xC4 -> step_zero_inst cpu cpy_op
            | 0xC5 -> step_zero_inst cpu cmp_op
            | 0xC6 -> step_zero_inst cpu dec_op
            | 0xC7 -> step_zero_inst cpu dcp_op
            | 0xE4 -> step_zero_inst cpu cpx_op
            | 0xE5 -> step_zero_inst cpu sbc_op
            | 0xE6 -> step_zero_inst cpu inc_op
            | 0xE7 -> step_zero_inst cpu isc_op
            | _ -> nop_op cpu

        let step_zerx (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x14 -> step_zerx_inst cpu dop_op
            | 0x15 -> step_zerx_inst cpu ora_op
            | 0x16 -> step_zerx_inst cpu asl_op
            | 0x17 -> step_zerx_inst cpu slo_op
            | 0x34 -> step_zerx_inst cpu dop_op
            | 0x35 -> step_zerx_inst cpu and_op
            | 0x36 -> step_zerx_inst cpu rol_op
            | 0x37 -> step_zerx_inst cpu rla_op
            | 0x54 -> step_zerx_inst cpu dop_op
            | 0x55 -> step_zerx_inst cpu eor_op
            | 0x56 -> step_zerx_inst cpu lsr_op
            | 0x57 -> step_zerx_inst cpu sre_op
            | 0x74 -> step_zerx_inst cpu dop_op
            | 0x75 -> step_zerx_inst cpu adc_op
            | 0x76 -> step_zerx_inst cpu ror_op
            | 0x77 -> step_zerx_inst cpu rra_op
            | 0x94 -> step_zerx_inst cpu sty_op
            | 0x95 -> step_zerx_inst cpu sta_op
            | 0xB4 -> step_zerx_inst cpu ldy_op
            | 0xB5 -> step_zerx_inst cpu lda_op
            | 0xD4 -> step_zerx_inst cpu dop_op
            | 0xD5 -> step_zerx_inst cpu cmp_op
            | 0xD6 -> step_zerx_inst cpu dec_op
            | 0xD7 -> step_zerx_inst cpu dcp_op
            | 0xF4 -> step_zerx_inst cpu dop_op
            | 0xF5 -> step_zerx_inst cpu sbc_op
            | 0xF6 -> step_zerx_inst cpu inc_op
            | 0xF7 -> step_zerx_inst cpu isc_op
            | _ -> nop_op cpu

        let step_zery (opcode : int) (cpu : t) : t =
            match opcode with
            | 0x96 -> step_zery_inst cpu stx_op
            | 0x97 -> step_zery_inst cpu sax_op
            | 0xB6 -> step_zery_inst cpu ldx_op
            | 0xB7 -> step_zery_inst cpu lax_op
            | _ -> nop_op cpu
    end

    open Decode

    let step (cpu : t) : t =
        let opcode = fetch_ui8_op cpu |> ( ~* ) in
        let stepped_cpu =
            increment_pc cpu 1 |> step_none opcode |> step_accm opcode
            |> step_abst opcode |> step_absx opcode |> step_absy opcode
            |> step_imed opcode |> step_indr opcode |> step_xind opcode
            |> step_indy opcode |> step_relt opcode |> step_zero opcode
            |> step_zerx opcode |> step_zery opcode
        in
        match Bus.poll_interrupt cpu.bus with
        | Some IRQ ->
            if not stepped_cpu.flags.interrupt_disable then irq stepped_cpu
            else stepped_cpu
        | Some NMI -> nmi stepped_cpu
        | Some RESET -> reset stepped_cpu
        | None -> stepped_cpu
end
