open Cpu
open UInt8
open Decode
open UInt16

module Instruction = struct
    let adc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let operand = Decode.contents cpu mode in
        let summed_acc = cpu.accumulator ++ operand ++ ?.(cpu.flags.carr_bit) in
        let overflow = Decode.overflow operand cpu.accumulator summed_acc in
        let neg_bit = ?-summed_acc in
        let zero_bit = ?*summed_acc in
        {
            cpu with
            accumulator = summed_acc;
            flags =
                {
                    cpu.flags with
                    zero = zero_bit;
                    negative = neg_bit;
                    carr_bit = overflow;
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
        let cpu_pushed_pc = CPU.push_stack_ui16 cpu cpu.program_counter in
        let uint8_flags = CPU.flags_ui8 cpu in
        let cpu_pushed_flags = CPU.push_stack_ui8 cpu_pushed_pc uint8_flags in
        let interrupt_vector = CPU.fetch_ui16 cpu_pushed_flags ~^0xFFFE in
        {
            cpu_pushed_flags with
            program_counter = interrupt_vector;
            flags = { cpu_pushed_flags.flags with break = true };
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
        { cpu with flags = { cpu.flags with zero = false } }

    let cld_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with decimal = false } }

    let cli_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with interrupt = false } }

    let clv_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with overflow = false } }

    let cmp_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let mem_contents = Decode.contents cpu mode in
        let cmp_mem_acc = cpu.accumulator -- mem_contents in
        let carry_flag = cmp_mem_acc <?> ~.0 > 0 in
        let zero_flag = ?*cmp_mem_acc in
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
        let carry_flag = cmp_mem_x <?> ~.0 > 0 in
        let zero_flag = ?*cmp_mem_x in
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
        let carry_flag = cmp_mem_x <?> ~.0 > 0 in
        let zero_flag = ?*cmp_mem_x in
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

    (* TODO: Finish implemetning this function. *)
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

    (* TODO: Finish implemetning this function. *)
    let pha_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implementing this function. *)
    let php_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let pla_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let plp_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let rol_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        cpu

    (* TODO: Finish implemetning this function. *)
    let ror_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        cpu

    (* TODO: Finish implemetning this function. *)
    let rti_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let rts_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let sbc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        cpu

    (* TODO: Finish implemetning this function. *)
    let sec_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let sed_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let sei_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let sta_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        cpu

    (* TODO: Finish implemetning this function. *)
    let stx_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        cpu

    (* TODO: Finish implemetning this function. *)
    let sty_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        cpu

    (* TODO: Finish implemetning this function. *)
    let tax_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let tay_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let tsx_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let txa_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let txs_op (cpu : CPU.t) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let tya_op (cpu : CPU.t) : CPU.t = cpu
end
