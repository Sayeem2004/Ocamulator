open Cpu
open UInt8
open Decode
open UInt16

module Instruction = struct
    let adc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let operand = Decode.contents cpu mode in
        let sum_fir = cpu.accumulator ++ operand in
        let sum_sec = sum_fir ++ ?.(cpu.flags.carr_bit) in
        let carr_bit =
            Decode.add_unsigned_overflow cpu.accumulator operand
            || Decode.add_unsigned_overflow sum_fir ?.(cpu.flags.carr_bit)
        in
        let overflow =
            Decode.add_signed_overflow cpu.accumulator operand
            || Decode.add_signed_overflow sum_fir ?.(cpu.flags.carr_bit)
        in
        let neg_bit = ?-sum_sec in
        let zero_bit = ?*sum_sec in
        {
            cpu with
            accumulator = sum_sec;
            flags =
                {
                    cpu.flags with
                    zero = zero_bit;
                    negative = neg_bit;
                    carr_bit;
                    overflow;
                };
        }

    let and_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let operand = Decode.contents cpu mode in
        let modif_acc = cpu.accumulator &&. operand in
        let zero_bit = ?*modif_acc in
        let neg_bit = ?-modif_acc in
        {
            cpu with
            accumulator = modif_acc;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let asl_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        match mode with
        | Accumulator ->
            let shifted_acc = cpu.accumulator << 1 in
            let carry_bit = ?-(cpu.accumulator) in
            let zero_bit = ?*shifted_acc in
            let neg_bit = ?-shifted_acc in
            {
                cpu with
                accumulator = shifted_acc;
                flags =
                    {
                        cpu.flags with
                        zero = zero_bit;
                        carr_bit = carry_bit;
                        negative = neg_bit;
                    };
            }
        | addr_mode ->
            let operand_addr = Decode.address cpu addr_mode in
            let operand_contents = Decode.contents cpu addr_mode in
            let shifted_contents = operand_contents << 1 in
            let carry_bit = ?-operand_contents in
            let zero_bit = ?*shifted_contents in
            let neg_bit = ?-shifted_contents in
            CPU.write_ui8 cpu operand_addr shifted_contents;
            {
                cpu with
                flags =
                    {
                        cpu.flags with
                        zero = zero_bit;
                        carr_bit = carry_bit;
                        negative = neg_bit;
                    };
            }

    let bcc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if not cpu.flags.carr_bit then
            let branch_addr = Decode.address cpu mode in
            { cpu with program_counter = branch_addr }
        else cpu

    let bcs_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if cpu.flags.carr_bit then
            let branch_addr = Decode.address cpu mode in
            { cpu with program_counter = branch_addr }
        else cpu

    let beq_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if cpu.flags.zero then
            let branch_addr = Decode.address cpu mode in
            { cpu with program_counter = branch_addr }
        else cpu

    let bit_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let mem_contents = Decode.contents cpu mode in
        let zero_bit = ?*(mem_contents &&. cpu.accumulator) in
        let bit_6 = ?-(mem_contents << 1) in
        let bit_7 = ?-mem_contents in
        {
            cpu with
            flags =
                { cpu.flags with zero = zero_bit; overflow = bit_6; negative = bit_7 };
        }

    let bmi_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if cpu.flags.negative then
            let branch_addr = Decode.address cpu mode in
            { cpu with program_counter = branch_addr }
        else cpu

    let bne_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if not cpu.flags.zero then
            let branch_addr = Decode.address cpu mode in
            { cpu with program_counter = branch_addr }
        else cpu

    let bpl_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if not cpu.flags.negative then
            let branch_addr = Decode.address cpu mode in
            { cpu with program_counter = branch_addr }
        else cpu

    let brk_op (cpu : CPU.t) : CPU.t =
        let cpu_pushed_pc = CPU.push_stack_ui16 cpu (cpu.program_counter +++ ~^1) in
        let uint8_flags = CPU.flags_ui8 { cpu_pushed_pc.flags with break = true } in
        let cpu_pushed_flags = CPU.push_stack_ui8 cpu_pushed_pc uint8_flags in
        let interrupt_vector = CPU.fetch_ui16 cpu_pushed_flags ~^0xFFFE in
        {
            cpu_pushed_flags with
            program_counter = interrupt_vector;
            flags = { cpu_pushed_flags.flags with interrupt = true };
        }

    let bvc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if not cpu.flags.overflow then
            let branch_addr = Decode.address cpu mode in
            { cpu with program_counter = branch_addr }
        else cpu

    let bvs_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if cpu.flags.overflow then
            let branch_addr = Decode.address cpu mode in
            { cpu with program_counter = branch_addr }
        else cpu

    let clc_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with carr_bit = false } }

    let cld_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with decimal = false } }

    let cli_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with interrupt = false } }

    let clv_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with overflow = false } }

    let cmp_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let mem_contents = Decode.contents cpu mode in
        let cmp_mem_acc = cpu.accumulator -- mem_contents in
        let carry_flag = cpu.accumulator <?> mem_contents >= 0 in
        let zero_flag = cpu.accumulator == mem_contents in
        let neg_flag = ?-cmp_mem_acc in
        {
            cpu with
            flags =
                {
                    cpu.flags with
                    carr_bit = carry_flag;
                    zero = zero_flag;
                    negative = neg_flag;
                };
        }

    let cpx_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let mem_contents = Decode.contents cpu mode in
        let cmp_mem_x = cpu.register_X -- mem_contents in
        let carry_flag = cpu.register_X <?> mem_contents >= 0 in
        let zero_flag = cpu.register_X == mem_contents in
        let neg_flag = ?-cmp_mem_x in
        {
            cpu with
            flags =
                {
                    cpu.flags with
                    carr_bit = carry_flag;
                    zero = zero_flag;
                    negative = neg_flag;
                };
        }

    let cpy_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let mem_contents = Decode.contents cpu mode in
        let cmp_mem_x = cpu.register_Y -- mem_contents in
        let carry_flag = cpu.register_Y <?> mem_contents >= 0 in
        let zero_flag = cpu.register_Y == mem_contents in
        let neg_flag = ?-cmp_mem_x in
        {
            cpu with
            flags =
                {
                    cpu.flags with
                    carr_bit = carry_flag;
                    zero = zero_flag;
                    negative = neg_flag;
                };
        }

    let dec_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let mem_contents = Decode.contents cpu mode in
        let mem_addr = Decode.address cpu mode in
        let dec_contents = mem_contents -- ~.0x01 in
        let zero_flag = ?*dec_contents in
        let neg_flag = ?-dec_contents in
        CPU.write_ui8 cpu mem_addr dec_contents;
        {
            cpu with
            flags = { cpu.flags with zero = zero_flag; negative = neg_flag };
        }

    let dex_op (cpu : CPU.t) : CPU.t =
        let dec_x = cpu.register_X -- ~.0x01 in
        let zero_flag = ?*dec_x in
        let neg_flag = ?-dec_x in
        {
            cpu with
            register_X = dec_x;
            flags = { cpu.flags with zero = zero_flag; negative = neg_flag };
        }

    let dey_op (cpu : CPU.t) : CPU.t =
        let dec_y = cpu.register_Y -- ~.0x01 in
        let zero_flag = ?*dec_y in
        let neg_flag = ?-dec_y in
        {
            cpu with
            register_Y = dec_y;
            flags = { cpu.flags with zero = zero_flag; negative = neg_flag };
        }

    let eor_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let operand = Decode.contents cpu mode in
        let xor_acc = cpu.accumulator |/. operand in
        let zero_bit = ?*xor_acc in
        let neg_bit = ?-xor_acc in
        {
            cpu with
            accumulator = xor_acc;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let inc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let mem_contents = Decode.contents cpu mode in
        let mem_addr = Decode.address cpu mode in
        let inc_contents = mem_contents ++ ~.0x01 in
        let zero_flag = ?*inc_contents in
        let neg_flag = ?-inc_contents in
        CPU.write_ui8 cpu mem_addr inc_contents;
        {
            cpu with
            flags = { cpu.flags with zero = zero_flag; negative = neg_flag };
        }

    let inx_op (cpu : CPU.t) : CPU.t =
        let inc_x = cpu.register_X ++ ~.0x01 in
        let zero_flag = ?*inc_x in
        let neg_flag = ?-inc_x in
        {
            cpu with
            register_X = inc_x;
            flags = { cpu.flags with zero = zero_flag; negative = neg_flag };
        }

    let iny_op (cpu : CPU.t) : CPU.t =
        let inc_y = cpu.register_Y ++ ~.0x01 in
        let zero_flag = ?*inc_y in
        let neg_flag = ?-inc_y in
        {
            cpu with
            register_Y = inc_y;
            flags = { cpu.flags with zero = zero_flag; negative = neg_flag };
        }

    let jmp_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let jmp_addr = Decode.address cpu mode in
        { cpu with program_counter = jmp_addr }

    let jsr_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let return_point = cpu.program_counter --- ~^0x0001 in
        let pushed_cpu = CPU.push_stack_ui16 cpu return_point in
        let jmp_addr = Decode.address pushed_cpu mode in
        { pushed_cpu with program_counter = jmp_addr }

    let lda_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let mem_contents = Decode.contents cpu mode in
        let zero_flag = ?*mem_contents in
        let neg_flag = ?-mem_contents in
        {
            cpu with
            accumulator = mem_contents;
            flags = { cpu.flags with zero = zero_flag; negative = neg_flag };
        }

    let ldx_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let mem_contents = Decode.contents cpu mode in
        let zero_flag = ?*mem_contents in
        let neg_flag = ?-mem_contents in
        {
            cpu with
            register_X = mem_contents;
            flags = { cpu.flags with zero = zero_flag; negative = neg_flag };
        }

    let ldy_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let mem_contents = Decode.contents cpu mode in
        let zero_flag = ?*mem_contents in
        let neg_flag = ?-mem_contents in
        {
            cpu with
            register_Y = mem_contents;
            flags = { cpu.flags with zero = zero_flag; negative = neg_flag };
        }

    let lsr_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        match mode with
        | Accumulator ->
            let shifted_acc = cpu.accumulator >> 1 in
            let carry_bit = ?+(cpu.accumulator) in
            let zero_bit = ?*shifted_acc in
            let neg_bit = ?-shifted_acc in
            {
                cpu with
                accumulator = shifted_acc;
                flags =
                    {
                        cpu.flags with
                        zero = zero_bit;
                        carr_bit = carry_bit;
                        negative = neg_bit;
                    };
            }
        | addr_mode ->
            let operand_addr = Decode.address cpu addr_mode in
            let operand_contents = Decode.contents cpu addr_mode in
            let shifted_contents = operand_contents >> 1 in
            let carry_bit = ?+operand_contents in
            let zero_bit = ?*shifted_contents in
            let neg_bit = ?-shifted_contents in
            CPU.write_ui8 cpu operand_addr shifted_contents;
            {
                cpu with
                flags =
                    {
                        cpu.flags with
                        zero = zero_bit;
                        carr_bit = carry_bit;
                        negative = neg_bit;
                    };
            }

    let nop_op (cpu : CPU.t) : CPU.t = cpu

    let ora_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let operand = Decode.contents cpu mode in
        let or_acc = cpu.accumulator ||. operand in
        let zero_bit = ?*or_acc in
        let neg_bit = ?-or_acc in
        {
            cpu with
            accumulator = or_acc;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let pha_op (cpu : CPU.t) : CPU.t = CPU.push_stack_ui8 cpu cpu.accumulator

    let php_op (cpu : CPU.t) : CPU.t =
        let proc_status_flags = { cpu.flags with break = true; reserved = true } in
        CPU.push_stack_ui8 cpu (CPU.flags_ui8 proc_status_flags)

    let pla_op (cpu : CPU.t) : CPU.t =
        let pulled_acc = CPU.peek_stack_ui8 cpu in
        let popped_cpu = CPU.pop_stack_ui8 cpu in
        let zero_bit = ?*pulled_acc in
        let neg_bit = ?-pulled_acc in
        {
            popped_cpu with
            accumulator = pulled_acc;
            flags = { popped_cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let plp_op (cpu : CPU.t) : CPU.t =
        let pulled_flags = CPU.peek_stack_ui8 cpu in
        let popped_cpu = CPU.pop_stack_ui8 cpu in
        let new_flags = CPU.flags_from_ui8 pulled_flags in
        {
            popped_cpu with
            flags =
                {
                    popped_cpu.flags with
                    overflow = new_flags.overflow;
                    carr_bit = new_flags.carr_bit;
                    negative = new_flags.negative;
                    decimal = new_flags.decimal;
                    interrupt = new_flags.interrupt;
                    zero = new_flags.zero;
                };
        }

    let rol_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        match mode with
        | Accumulator ->
            let shifted_acc = (cpu.accumulator << 1) ++ ?.(cpu.flags.carr_bit) in
            let carry_bit = ?-(cpu.accumulator) in
            let zero_bit = ?*shifted_acc in
            let neg_bit = ?-shifted_acc in
            {
                cpu with
                accumulator = shifted_acc;
                flags =
                    {
                        cpu.flags with
                        zero = zero_bit;
                        carr_bit = carry_bit;
                        negative = neg_bit;
                    };
            }
        | addr_mode ->
            let operand_addr = Decode.address cpu addr_mode in
            let operand_contents = Decode.contents cpu addr_mode in
            let shifted_contents =
                (operand_contents << 1) ++ ?.(cpu.flags.carr_bit)
            in
            let carry_bit = ?-operand_contents in
            let zero_bit = ?*shifted_contents in
            let neg_bit = ?-shifted_contents in
            CPU.write_ui8 cpu operand_addr shifted_contents;
            {
                cpu with
                flags =
                    {
                        cpu.flags with
                        zero = zero_bit;
                        carr_bit = carry_bit;
                        negative = neg_bit;
                    };
            }

    let ror_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        match mode with
        | Accumulator ->
            let shifted_acc =
                cpu.accumulator >> 1 &&. ~.0b01111111 ||. (?.(cpu.flags.carr_bit) << 7)
            in
            let carry_bit = ?+(cpu.accumulator) in
            let zero_bit = ?*shifted_acc in
            let neg_bit = ?-shifted_acc in
            {
                cpu with
                accumulator = shifted_acc;
                flags =
                    {
                        cpu.flags with
                        zero = zero_bit;
                        carr_bit = carry_bit;
                        negative = neg_bit;
                    };
            }
        | addr_mode ->
            let operand_addr = Decode.address cpu addr_mode in
            let operand_contents = Decode.contents cpu addr_mode in
            let shifted_contents =
                operand_contents >> 1 &&. ~.0b01111111
                                    ||. (?.(cpu.flags.carr_bit) << 7)
            in
            let carry_bit = ?+operand_contents in
            let zero_bit = ?*shifted_contents in
            let neg_bit = ?-shifted_contents in
            CPU.write_ui8 cpu operand_addr shifted_contents;
            {
                cpu with
                flags =
                    {
                        cpu.flags with
                        zero = zero_bit;
                        carr_bit = carry_bit;
                        negative = neg_bit;
                    };
            }

    let rti_op (cpu : CPU.t) : CPU.t =
        let stack_flags_ui8 = CPU.peek_stack_ui8 cpu in
        let stack_flags = CPU.flags_from_ui8 stack_flags_ui8 in
        let popped_flags_cpu = CPU.pop_stack_ui8 cpu in
        let pc = CPU.peek_stack_ui16 popped_flags_cpu in
        let popped_pc_cpu = CPU.pop_stack_ui16 popped_flags_cpu in
        {
            popped_pc_cpu with
            flags =
                {
                    popped_flags_cpu.flags with
                    negative = stack_flags.negative;
                    zero = stack_flags.zero;
                    carr_bit = stack_flags.carr_bit;
                    interrupt = stack_flags.interrupt;
                    decimal = stack_flags.decimal;
                    overflow = stack_flags.overflow;
                };
            program_counter = pc;
        }

    let rts_op (cpu : CPU.t) : CPU.t =
        let pc = CPU.peek_stack_ui16 cpu +++ ~^1 in
        let popped_cpu = CPU.pop_stack_ui16 cpu in
        { popped_cpu with program_counter = pc }

    let sbc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let operand = Decode.contents cpu mode in
        let sub_sum = operand ++ ?.(not cpu.flags.carr_bit) in
        let sub_res = cpu.accumulator -- sub_sum in
        let overflow = Decode.sub_signed_overflow cpu.accumulator sub_sum in
        let carr_bit = not (Decode.sub_unsigned_overflow cpu.accumulator sub_sum) in
        let zero_bit = ?*sub_res in
        let neg_bit = ?-sub_res in
        {
            cpu with
            accumulator = sub_res;
            flags =
                {
                    cpu.flags with
                    zero = zero_bit;
                    carr_bit;
                    overflow;
                    negative = neg_bit;
                };
        }

    let sec_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with carr_bit = true } }

    let sed_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with decimal = true } }

    let sei_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with interrupt = true } }

    let sta_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        match mode with
        | addr_mode ->
            let operand_addr = Decode.address cpu addr_mode in
            CPU.write_ui8 cpu operand_addr cpu.accumulator;
            cpu

    let stx_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        match mode with
        | addr_mode ->
            let operand_addr = Decode.address cpu addr_mode in
            CPU.write_ui8 cpu operand_addr cpu.register_X;
            cpu

    let sty_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        match mode with
        | addr_mode ->
            let operand_addr = Decode.address cpu addr_mode in
            CPU.write_ui8 cpu operand_addr cpu.register_Y;
            cpu

    let tax_op (cpu : CPU.t) : CPU.t =
        let zero_bit = ?*(cpu.accumulator) in
        let neg_bit = ?-(cpu.accumulator) in
        {
            cpu with
            register_X = cpu.accumulator;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let tay_op (cpu : CPU.t) : CPU.t =
        let zero_bit = ?*(cpu.accumulator) in
        let neg_bit = ?-(cpu.accumulator) in
        {
            cpu with
            register_Y = cpu.accumulator;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let tsx_op (cpu : CPU.t) : CPU.t =
        let zero_bit = ?*(cpu.stack_pointer) in
        let neg_bit = ?-(cpu.stack_pointer) in
        {
            cpu with
            register_X = cpu.stack_pointer;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let txa_op (cpu : CPU.t) : CPU.t =
        let zero_bit = ?*(cpu.register_X) in
        let neg_bit = ?-(cpu.register_X) in
        {
            cpu with
            accumulator = cpu.register_X;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let txs_op (cpu : CPU.t) : CPU.t = { cpu with stack_pointer = cpu.register_X }

    let tya_op (cpu : CPU.t) : CPU.t =
        let zero_bit = ?*(cpu.register_Y) in
        let neg_bit = ?-(cpu.register_Y) in
        {
            cpu with
            accumulator = cpu.register_Y;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let dop_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        cpu

    let top_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        cpu

    let alr_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        and_op mode cpu |> lsr_op Accumulator

    let anc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let operand = Decode.contents cpu mode in
        let modif_acc = cpu.accumulator &&. operand in
        let zero_bit = ?*modif_acc in
        let neg_bit = ?-modif_acc in
        {
            cpu with
            accumulator = modif_acc;
            flags =
                {
                    cpu.flags with
                    zero = zero_bit;
                    negative = neg_bit;
                    carr_bit = neg_bit;
                };
        }

    let sax_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let and_val = cpu.accumulator &&. cpu.register_X in
        let addr = Decode.address cpu mode in
        CPU.write_ui8 cpu addr and_val;
        cpu

    let ane_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let operand = Decode.contents cpu mode in
        let modif_acc = ~.0xEE ||. cpu.accumulator &&. cpu.register_X &&. operand in
        let zero_bit = ?*modif_acc in
        let neg_bit = ?-modif_acc in
        {
            cpu with
            accumulator = modif_acc;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let lax_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        lda_op mode cpu |> ldx_op mode

    let las_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let operand = Decode.contents cpu mode in
        let value = cpu.stack_pointer &&. operand in
        let zero_bit = ?*value in
        let neg_bit = ?-value in
        {
            cpu with
            accumulator = value;
            register_X = value;
            stack_pointer = value;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let dcp_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        dec_op mode cpu |> cmp_op mode

    let lxa_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let operand = Decode.contents cpu mode in
        let modif_acc = ~.0xEE ||. cpu.accumulator &&. operand in
        let zero_bit = ?*modif_acc in
        let neg_bit = ?-modif_acc in
        {
            cpu with
            accumulator = modif_acc;
            register_X = modif_acc;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit };
        }

    let isc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        inc_op mode cpu |> sbc_op mode

    let rla_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        rol_op mode cpu |> and_op mode

    let slo_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        asl_op mode cpu |> ora_op mode

    let sre_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        lsr_op mode cpu |> eor_op mode

    let jam_op (cpu : CPU.t) : CPU.t =
        { cpu with program_counter = cpu.program_counter --- ~^0x0001 }

    let rra_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        ror_op mode cpu |> adc_op mode

    let sbx_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let sub_sec = Decode.contents cpu mode in
        let sub_fir = cpu.accumulator &&. cpu.register_X in
        let sub_res = sub_fir -- sub_sec in
        let carr_bit = sub_fir <?> sub_res >= 0 in
        let zero_bit = ?*sub_res in
        let neg_bit = ?-sub_res in
        {
            cpu with
            register_X = sub_res;
            flags = { cpu.flags with zero = zero_bit; negative = neg_bit; carr_bit };
        }

    let sha_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let addr = Decode.address cpu mode in
        let high = !. ((addr &&& ~^ 0xFF00) >>> 8) in
        let value = cpu.accumulator &&. cpu.register_X &&. (high ++ ~. 0x01) in
        CPU.write_ui8 cpu addr value;
        cpu

    let tas_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let stack_val = cpu.accumulator &&. cpu.register_X in
        let new_cpu = { cpu with stack_pointer = stack_val } in
        let addr = Decode.address new_cpu mode in
        let high = !. ((addr &&& ~^ 0xFF00) >>> 8) in
        let value = stack_val &&. (high ++ ~. 0x01) in
        CPU.write_ui8 new_cpu addr value;
        new_cpu

    let shy_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let addr = Decode.address cpu mode in
        let high = !. ((addr &&& ~^ 0xFF00) >>> 8) in
        let value = cpu.register_Y &&. (high ++ ~. 0x01) in
        CPU.write_ui8 cpu addr value;
        cpu

    let shx_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let addr = Decode.address cpu mode in
        let high = !. ((addr &&& ~^ 0xFF00) >>> 8) in
        let value = cpu.register_X &&. (high ++ ~. 0x01) in
        CPU.write_ui8 cpu addr value;
        cpu

    let arr_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let value = (cpu.accumulator &&. Decode.contents cpu mode) >> 1 in
        let value = if cpu.flags.carr_bit then value ||. ~.0x80 else value in
        let carr_bit = not ((value &&. ~.0x01) <-> ~. 0) in
        let zero_bit = value <-> ~. 0 in
        let ngtv_bit = not ((value &&. ~.0x80) <-> ~. 0) in
        let over_bit = not ((value &&. (value << 1) &&. ~. 0x40) <-> ~. 0) in
        {
            cpu with
            accumulator = value;
            flags =
                {
                    cpu.flags with
                    zero = zero_bit;
                    negative = ngtv_bit;
                    carr_bit = carr_bit;
                    overflow = over_bit;
                };
        }
end
