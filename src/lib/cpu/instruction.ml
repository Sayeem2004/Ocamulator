open Cpu
open UInt8
open Decode
open UInt16

module Instruction = struct
    let adc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let sum = ~*(cpu.accumulator) + ~*contents + ~*(?.(cpu.flags.carry)) in
        let res, acc = (~.sum, cpu.accumulator) in
        let carr, zero, ngtv = (sum > 0xFF, ?*res, ?-res) in
        let over = ?@(acc |/. contents) &&. (acc |/. res) &&. ~.0x80 > ~.0x00 in
        {
            cpu with
            accumulator = res;
            flags =
                { cpu.flags with carry = carr; zero; overflow = over; negative = ngtv };
        }

    let and_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let operand = Decode.contents cpu mode in
        let modif_acc = cpu.accumulator &&. operand in
        let zero, ngtv = (?*modif_acc, ?-modif_acc) in
        {
            cpu with
            accumulator = modif_acc;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let asl_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
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
            let address = Decode.address cpu addr_mode in
            let contents = Decode.contents cpu addr_mode in
            let shifted = contents << 1 in
            let carr, zero, ngtv = (?-contents, ?*shifted, ?-shifted) in
            CPU.write_ui8 cpu address shifted;
            {
                cpu with
                flags = { cpu.flags with carry = carr; zero; negative = ngtv };
            }

    let bcc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if not cpu.flags.carry then
            { cpu with progCounter = Decode.address cpu mode }
        else cpu

    let bcs_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if cpu.flags.carry then { cpu with progCounter = Decode.address cpu mode }
        else cpu

    let beq_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if cpu.flags.zero then { cpu with progCounter = Decode.address cpu mode }
        else cpu

    let bit_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let mem_contents = Decode.contents cpu mode in
        let zero = ?*(mem_contents &&. cpu.accumulator) in
        let bit_6, bit_7 = (?-(mem_contents << 1), ?-mem_contents) in
        {
            cpu with
            flags = { cpu.flags with zero; overflow = bit_6; negative = bit_7 };
        }

    let bmi_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if cpu.flags.negative then
            { cpu with progCounter = Decode.address cpu mode }
        else cpu

    let bne_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if not cpu.flags.zero then
            { cpu with progCounter = Decode.address cpu mode }
        else cpu

    let bpl_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if not cpu.flags.negative then
            { cpu with progCounter = Decode.address cpu mode }
        else cpu

    let brk_op (cpu : CPU.t) : CPU.t =
        let cpu_pc = CPU.push_stack_ui16 cpu (cpu.progCounter +++ ~^1) in
        let flags = CPU.flags_to_ui8 { cpu_pc.flags with break = true } in
        let cpu_flags = CPU.push_stack_ui8 cpu_pc flags in
        let vector = CPU.fetch_ui16 cpu_flags ~^0xFFFE in
        {
            cpu_flags with
            progCounter = vector;
            flags = { cpu_flags.flags with interrupt = true };
        }

    let bvc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if not cpu.flags.overflow then
            { cpu with progCounter = Decode.address cpu mode }
        else cpu

    let bvs_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        if cpu.flags.overflow then
            { cpu with progCounter = Decode.address cpu mode }
        else cpu

    let clc_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with carry = false } }

    let cld_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with decimal = false } }

    let cli_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with interrupt = false } }

    let clv_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with overflow = false } }

    let cmp_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let sub_acc = cpu.accumulator -- contents in
        let carr = cpu.accumulator <?> contents >= 0 in
        let zero = cpu.accumulator == contents in
        let ngtv = ?-sub_acc in
        { cpu with flags = { cpu.flags with carry = carr; zero; negative = ngtv } }

    let cpx_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let subX = cpu.registerX -- contents in
        let carr = cpu.registerX <?> contents >= 0 in
        let zero = cpu.registerX == contents in
        let ngtv = ?-subX in
        { cpu with flags = { cpu.flags with carry = carr; zero; negative = ngtv } }

    let cpy_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let subY = cpu.registerY -- contents in
        let carr = cpu.registerY <?> contents >= 0 in
        let zero = cpu.registerY == contents in
        let ngtv = ?-subY in
        { cpu with flags = { cpu.flags with carry = carr; zero; negative = ngtv } }

    let dec_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let address = Decode.address cpu mode in
        let decrement = contents -- ~.0x01 in
        let zero, ngtv = (?*decrement, ?-decrement) in
        CPU.write_ui8 cpu address decrement;
        { cpu with flags = { cpu.flags with zero; negative = ngtv } }

    let dex_op (cpu : CPU.t) : CPU.t =
        let decrement = cpu.registerX -- ~.0x01 in
        let zero, ngtv = (?*decrement, ?-decrement) in
        {
            cpu with
            registerX = decrement;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let dey_op (cpu : CPU.t) : CPU.t =
        let decrement = cpu.registerY -- ~.0x01 in
        let zero, ngtv = (?*decrement, ?-decrement) in
        {
            cpu with
            registerY = decrement;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let eor_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let xor_acc = cpu.accumulator |/. contents in
        let zero, ngtv = (?*xor_acc, ?-xor_acc) in
        {
            cpu with
            accumulator = xor_acc;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let inc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let address = Decode.address cpu mode in
        let increment = contents ++ ~.0x01 in
        let zero, ngtv = (?*increment, ?-increment) in
        CPU.write_ui8 cpu address increment;
        { cpu with flags = { cpu.flags with zero; negative = ngtv } }

    let inx_op (cpu : CPU.t) : CPU.t =
        let increment = cpu.registerX ++ ~.0x01 in
        let zero, ngtv = (?*increment, ?-increment) in
        {
            cpu with
            registerX = increment;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let iny_op (cpu : CPU.t) : CPU.t =
        let increment = cpu.registerY ++ ~.0x01 in
        let zero, ngtv = (?*increment, ?-increment) in
        {
            cpu with
            registerY = increment;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let jmp_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        { cpu with progCounter = Decode.address cpu mode }

    let jsr_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let return = cpu.progCounter --- ~^0x01 in
        let pushed = CPU.push_stack_ui16 cpu return in
        { pushed with progCounter = Decode.address pushed mode }

    let lda_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let zero, ngtv = (?*contents, ?-contents) in
        {
            cpu with
            accumulator = contents;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let ldx_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let zero, ngtv = (?*contents, ?-contents) in
        {
            cpu with
            registerX = contents;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let ldy_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let zero, ngtv = (?*contents, ?-contents) in
        {
            cpu with
            registerY = contents;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let lsr_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
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
            let address = Decode.address cpu addr_mode in
            let contents = Decode.contents cpu addr_mode in
            let shifted = contents >> 1 in
            let carr, zero, ngtv = (?+contents, ?*shifted, ?-shifted) in
            CPU.write_ui8 cpu address shifted;
            {
                cpu with
                flags = { cpu.flags with carry = carr; zero; negative = ngtv };
            }

    let nop_op (cpu : CPU.t) : CPU.t = cpu

    let ora_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let or_acc = cpu.accumulator ||. contents in
        let zero, ngtv = (?*or_acc, ?-or_acc) in
        {
            cpu with
            accumulator = or_acc;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let pha_op (cpu : CPU.t) : CPU.t = CPU.push_stack_ui8 cpu cpu.accumulator

    let php_op (cpu : CPU.t) : CPU.t =
        let flags = { cpu.flags with break = true; reserved = true } in
        CPU.push_stack_ui8 cpu (CPU.flags_to_ui8 flags)

    let pla_op (cpu : CPU.t) : CPU.t =
        let pulled = CPU.peek_stack_ui8 cpu in
        let popped = CPU.pop_stack_ui8 cpu in
        let zero, ngtv = (?*pulled, ?-pulled) in
        {
            popped with
            accumulator = pulled;
            flags = { popped.flags with zero; negative = ngtv };
        }

    let plp_op (cpu : CPU.t) : CPU.t =
        let pulled = CPU.peek_stack_ui8 cpu in
        let popped = CPU.pop_stack_ui8 cpu in
        let flags = CPU.flags_from_ui8 pulled in
        { popped with flags = { flags with break = false; reserved = true } }

    let rol_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
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
            let address = Decode.address cpu addr_mode in
            let contents = Decode.contents cpu addr_mode in
            let shifted = (contents << 1) ++ ?.(cpu.flags.carry) in
            let carr, zero, ngtv = (?-contents, ?*shifted, ?-shifted) in
            CPU.write_ui8 cpu address shifted;
            {
                cpu with
                flags = { cpu.flags with carry = carr; zero; negative = ngtv };
            }

    let ror_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
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
            let address = Decode.address cpu addr_mode in
            let contents = Decode.contents cpu addr_mode in
            let shifted = contents >> 1 &&. ~.0x7F ||. (?.(cpu.flags.carry) << 7) in
            let carr, zero, ngtv = (?+contents, ?*shifted, ?-shifted) in
            CPU.write_ui8 cpu address shifted;
            {
                cpu with
                flags = { cpu.flags with carry = carr; zero; negative = ngtv };
            }

    let rti_op (cpu : CPU.t) : CPU.t =
        let flags = CPU.flags_from_ui8 (CPU.peek_stack_ui8 cpu) in
        let popped_flags = CPU.pop_stack_ui8 cpu in
        let counter = CPU.peek_stack_ui16 popped_flags in
        let popped_pc = CPU.pop_stack_ui16 popped_flags in
        {
            popped_pc with
            progCounter = counter;
            flags = { flags with break = false; reserved = true };
        }

    let rts_op (cpu : CPU.t) : CPU.t =
        let counter = CPU.peek_stack_ui16 cpu +++ ~^0x01 in
        let popped_pc = CPU.pop_stack_ui16 cpu in
        { popped_pc with progCounter = counter }

    let sbc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let sum = ~*(cpu.accumulator) + ~*(?@contents) + ~*(?.(cpu.flags.carry)) in
        let res, acc = (~.sum, cpu.accumulator) in
        let carr, zero, ngtv = (sum > 0xFF, ?*res, ?-res) in
        let over = acc |/. contents &&. (acc |/. res) &&. ~.0x80 > ~.0x00 in
        {
            cpu with
            accumulator = res;
            flags =
                { cpu.flags with carry = carr; zero; overflow = over; negative = ngtv };
        }

    let sec_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with carry = true } }

    let sed_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with decimal = true } }

    let sei_op (cpu : CPU.t) : CPU.t =
        { cpu with flags = { cpu.flags with interrupt = true } }

    let sta_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let address = Decode.address cpu mode in
        CPU.write_ui8 cpu address cpu.accumulator;
        cpu

    let stx_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let address = Decode.address cpu mode in
        CPU.write_ui8 cpu address cpu.registerX;
        cpu

    let sty_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let address = Decode.address cpu mode in
        CPU.write_ui8 cpu address cpu.registerY;
        cpu

    let tax_op (cpu : CPU.t) : CPU.t =
        let zero, ngtv = (?*(cpu.accumulator), ?-(cpu.accumulator)) in
        {
            cpu with
            registerX = cpu.accumulator;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let tay_op (cpu : CPU.t) : CPU.t =
        let zero, ngtv = (?*(cpu.accumulator), ?-(cpu.accumulator)) in
        {
            cpu with
            registerY = cpu.accumulator;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let tsx_op (cpu : CPU.t) : CPU.t =
        let zero, ngtv = (?*(cpu.stackPointer), ?-(cpu.stackPointer)) in
        {
            cpu with
            registerX = cpu.stackPointer;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let txa_op (cpu : CPU.t) : CPU.t =
        let zero, ngtv = (?*(cpu.registerX), ?-(cpu.registerX)) in
        {
            cpu with
            accumulator = cpu.registerX;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let txs_op (cpu : CPU.t) : CPU.t = { cpu with stackPointer = cpu.registerX }

    let tya_op (cpu : CPU.t) : CPU.t =
        let zero, ngtv = (?*(cpu.registerY), ?-(cpu.registerY)) in
        {
            cpu with
            accumulator = cpu.registerY;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let dop_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        cpu

    let top_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        cpu

    let alr_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        and_op mode cpu |> lsr_op Accumulator

    let anc_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let and_acc = cpu.accumulator &&. contents in
        let zero, ngtv = (?*and_acc, ?-and_acc) in
        {
            cpu with
            accumulator = and_acc;
            flags = { cpu.flags with zero; negative = ngtv; carry = ngtv };
        }

    let sax_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let and_acc = cpu.accumulator &&. cpu.registerX in
        CPU.write_ui8 cpu (Decode.address cpu mode) and_acc;
        cpu

    let ane_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let modif_acc = ~.0xEE ||. cpu.accumulator &&. cpu.registerX &&. contents in
        let zero, ngtv = (?*modif_acc, ?-modif_acc) in
        {
            cpu with
            accumulator = modif_acc;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let lax_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        lda_op mode cpu |> ldx_op mode

    let las_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let value = cpu.stackPointer &&. contents in
        let zero, ngtv = (?*value, ?-value) in
        {
            cpu with
            accumulator = value;
            registerX = value;
            stackPointer = value;
            flags = { cpu.flags with zero; negative = ngtv };
        }

    let dcp_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        dec_op mode cpu |> cmp_op mode

    let lxa_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let contents = Decode.contents cpu mode in
        let modif_acc = ~.0xEE ||. cpu.accumulator &&. contents in
        let zero, ngtv = (?*modif_acc, ?-modif_acc) in
        {
            cpu with
            accumulator = modif_acc;
            registerX = modif_acc;
            flags = { cpu.flags with zero; negative = ngtv };
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
        { cpu with progCounter = cpu.progCounter --- ~^0x01 }

    let rra_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        ror_op mode cpu |> adc_op mode

    let sbx_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let sub_sec = Decode.contents cpu mode in
        let sub_fir = cpu.accumulator &&. cpu.registerX in
        let sub_res = sub_fir -- sub_sec in
        let carr, zero, ngtv = (sub_fir <?> sub_res >= 0, ?*sub_res, ?-sub_res) in
        {
            cpu with
            registerX = sub_res;
            flags = { cpu.flags with carry = carr; zero; negative = ngtv };
        }

    let sha_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let address = Decode.address cpu mode in
        let high = !.(address &&& ~^0xFF00 >>> 8) in
        let value = cpu.accumulator &&. cpu.registerX &&. high ++ ~.0x01 in
        CPU.write_ui8 cpu address value;
        cpu

    let tas_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let stack_val = cpu.accumulator &&. cpu.registerX in
        let new_cpu = { cpu with stackPointer = stack_val } in
        let address = Decode.address new_cpu mode in
        let high = !.(address &&& ~^0xFF00 >>> 8) in
        let value = stack_val &&. high ++ ~.0x01 in
        CPU.write_ui8 new_cpu address value;
        new_cpu

    let shy_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let address = Decode.address cpu mode in
        let high = !.(address &&& ~^0xFF00 >>> 8) in
        let value = cpu.registerY &&. high ++ ~.0x01 in
        CPU.write_ui8 cpu address value;
        cpu

    let shx_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let address = Decode.address cpu mode in
        let high = !.(address &&& ~^0xFF00 >>> 8) in
        let value = cpu.registerX &&. high ++ ~.0x01 in
        CPU.write_ui8 cpu address value;
        cpu

    let arr_op (type a') (mode : a' Decode.memory_mode) (cpu : CPU.t) : CPU.t =
        let value = cpu.accumulator &&. Decode.contents cpu mode >> 1 in
        let value = if cpu.flags.carry then value ||. ~.0x80 else value in
        let carr, zero = (not (value &&. ~.0x01 <-> ~.0), value <-> ~.0) in
        let ngtv = not (value &&. ~.0x80 <-> ~.0) in
        let over = not (value &&. (value << 1) &&. ~.0x40 <-> ~.0) in
        {
            cpu with
            accumulator = value;
            flags =
                { cpu.flags with carry = carr; zero; negative = ngtv; overflow = over };
        }
end
