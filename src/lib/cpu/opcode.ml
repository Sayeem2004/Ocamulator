open Cpu
open UInt8
open Decode
open UInt16
open Instruction

module Opcode = struct
    type none_inst = CPU.t -> CPU.t
    type 'a some_inst = 'a Decode.memory_mode -> CPU.t -> CPU.t

    let step_none_inst (cpu : CPU.t) (inst : none_inst) : CPU.t = inst cpu

    let step_accm_inst (cpu : CPU.t) (inst : uint8 some_inst) : CPU.t =
        inst Accumulator cpu

    let step_abst_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let instr_cpu = inst (Absolute (Decode.fetch_uint16_op cpu)) cpu in
        Decode.incr_cpu_pc instr_cpu 2

    let step_absx_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let instr_cpu = inst (AbsoluteX (Decode.fetch_uint16_op cpu)) cpu in
        Decode.incr_cpu_pc instr_cpu 2

    let step_absy_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let instr_cpu = inst (AbsoluteY (Decode.fetch_uint16_op cpu)) cpu in
        Decode.incr_cpu_pc instr_cpu 2

    let step_imed_inst (cpu : CPU.t) (inst : uint8 some_inst) : CPU.t =
        let instr_cpu = inst (Immediate (Decode.fetch_uint8_op cpu)) cpu in
        Decode.incr_cpu_pc instr_cpu 1

    let step_indr_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let instr_cpu = inst (Indirect (Decode.fetch_uint16_op cpu)) cpu in
        Decode.incr_cpu_pc instr_cpu 2

    let step_xind_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let instr_cpu = inst (XIndirect (Decode.fetch_uint8_op cpu)) cpu in
        Decode.incr_cpu_pc instr_cpu 1

    let step_indy_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let instr_cpu = inst (IndirectY (Decode.fetch_uint8_op cpu)) cpu in
        Decode.incr_cpu_pc instr_cpu 1

    let step_relt_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let instr_cpu = inst (Relative (Decode.fetch_uint8_op cpu)) cpu in
        Decode.incr_cpu_pc instr_cpu 1

    let step_zero_inst (cpu : CPU.t) (inst : uint8 some_inst) : CPU.t =
        let instr_cpu = inst (Zeropage (Decode.fetch_uint8_op cpu)) cpu in
        Decode.incr_cpu_pc instr_cpu 1

    let step_zerx_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let instr_cpu = inst (ZeropageX (Decode.fetch_uint8_op cpu)) cpu in
        Decode.incr_cpu_pc instr_cpu 1

    let step_zery_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let instr_cpu = inst (ZeropageY (Decode.fetch_uint8_op cpu)) cpu in
        Decode.incr_cpu_pc instr_cpu 1

    let step_none (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x00 -> step_none_inst cpu Instruction.brk_op
        | 0x08 -> step_none_inst cpu Instruction.php_op
        | 0x18 -> step_none_inst cpu Instruction.clc_op
        | 0x28 -> step_none_inst cpu Instruction.plp_op
        | 0x38 -> step_none_inst cpu Instruction.sec_op
        | 0x40 -> step_none_inst cpu Instruction.rti_op
        | 0x48 -> step_none_inst cpu Instruction.pha_op
        | 0x58 -> step_none_inst cpu Instruction.cli_op
        | 0x60 -> step_none_inst cpu Instruction.rts_op
        | 0x68 -> step_none_inst cpu Instruction.pla_op
        | 0x78 -> step_none_inst cpu Instruction.sei_op
        | 0x88 -> step_none_inst cpu Instruction.dey_op
        | 0x8A -> step_none_inst cpu Instruction.txa_op
        | 0x98 -> step_none_inst cpu Instruction.tya_op
        | 0x9A -> step_none_inst cpu Instruction.txs_op
        | 0xA8 -> step_none_inst cpu Instruction.tay_op
        | 0xAA -> step_none_inst cpu Instruction.tax_op
        | 0xB8 -> step_none_inst cpu Instruction.clv_op
        | 0xBA -> step_none_inst cpu Instruction.tsx_op
        | 0xC8 -> step_none_inst cpu Instruction.iny_op
        | 0xCA -> step_none_inst cpu Instruction.dex_op
        | 0xD8 -> step_none_inst cpu Instruction.cld_op
        | 0xE8 -> step_none_inst cpu Instruction.inx_op
        | 0xEA -> step_none_inst cpu Instruction.nop_op
        | 0xF8 -> step_none_inst cpu Instruction.sed_op
        | _ -> cpu

    let step_accm (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x0A -> step_accm_inst cpu Instruction.asl_op
        | 0x2A -> step_accm_inst cpu Instruction.rol_op
        | 0x4A -> step_accm_inst cpu Instruction.lsr_op
        | 0x6A -> step_accm_inst cpu Instruction.ror_op
        | _ -> cpu

    let step_abst (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x0D -> step_abst_inst cpu Instruction.ora_op
        | 0x0E -> step_abst_inst cpu Instruction.asl_op
        | 0x20 -> step_abst_inst cpu Instruction.jsr_op
        | 0x2C -> step_abst_inst cpu Instruction.bit_op
        | 0x2D -> step_abst_inst cpu Instruction.and_op
        | 0x2E -> step_abst_inst cpu Instruction.rol_op
        | 0x4C -> step_abst_inst cpu Instruction.jmp_op
        | 0x4D -> step_abst_inst cpu Instruction.eor_op
        | 0x4E -> step_abst_inst cpu Instruction.lsr_op
        | 0x6D -> step_abst_inst cpu Instruction.adc_op
        | 0x6E -> step_abst_inst cpu Instruction.ror_op
        | 0x8C -> step_abst_inst cpu Instruction.sty_op
        | 0x8D -> step_abst_inst cpu Instruction.sta_op
        | 0x8E -> step_abst_inst cpu Instruction.stx_op
        | 0xAC -> step_abst_inst cpu Instruction.ldy_op
        | 0xAD -> step_abst_inst cpu Instruction.lda_op
        | 0xAE -> step_abst_inst cpu Instruction.ldx_op
        | 0xCC -> step_abst_inst cpu Instruction.cpy_op
        | 0xCD -> step_abst_inst cpu Instruction.cmp_op
        | 0xCE -> step_abst_inst cpu Instruction.dec_op
        | 0xEC -> step_abst_inst cpu Instruction.cpx_op
        | 0xED -> step_abst_inst cpu Instruction.sbc_op
        | 0xEE -> step_abst_inst cpu Instruction.inc_op
        | _ -> cpu

    let step_absx (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x1D -> step_absx_inst cpu Instruction.ora_op
        | 0x1E -> step_absx_inst cpu Instruction.asl_op
        | 0x3D -> step_absx_inst cpu Instruction.and_op
        | 0x3E -> step_absx_inst cpu Instruction.rol_op
        | 0x5D -> step_absx_inst cpu Instruction.eor_op
        | 0x5E -> step_absx_inst cpu Instruction.lsr_op
        | 0x7D -> step_absx_inst cpu Instruction.adc_op
        | 0x7E -> step_absx_inst cpu Instruction.ror_op
        | 0x9D -> step_absx_inst cpu Instruction.sta_op
        | 0xBC -> step_absx_inst cpu Instruction.ldy_op
        | 0xBD -> step_absx_inst cpu Instruction.lda_op
        | 0xDD -> step_absx_inst cpu Instruction.cmp_op
        | 0xDE -> step_absx_inst cpu Instruction.dec_op
        | 0xFD -> step_absx_inst cpu Instruction.sbc_op
        | 0xFE -> step_absx_inst cpu Instruction.inc_op
        | _ -> cpu

    let step_absy (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x19 -> step_absy_inst cpu Instruction.ora_op
        | 0x39 -> step_absy_inst cpu Instruction.and_op
        | 0x59 -> step_absy_inst cpu Instruction.eor_op
        | 0x79 -> step_absy_inst cpu Instruction.adc_op
        | 0x99 -> step_absy_inst cpu Instruction.sta_op
        | 0xB9 -> step_absy_inst cpu Instruction.lda_op
        | 0xBE -> step_absy_inst cpu Instruction.ldx_op
        | 0xD9 -> step_absy_inst cpu Instruction.cmp_op
        | 0xF9 -> step_absy_inst cpu Instruction.sbc_op
        | _ -> cpu

    let step_imed (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x09 -> step_imed_inst cpu Instruction.ora_op
        | 0x29 -> step_imed_inst cpu Instruction.and_op
        | 0x49 -> step_imed_inst cpu Instruction.eor_op
        | 0x69 -> step_imed_inst cpu Instruction.adc_op
        | 0xA0 -> step_imed_inst cpu Instruction.ldy_op
        | 0xA2 -> step_imed_inst cpu Instruction.ldx_op
        | 0xA9 -> step_imed_inst cpu Instruction.lda_op
        | 0xC0 -> step_imed_inst cpu Instruction.cpy_op
        | 0xC9 -> step_imed_inst cpu Instruction.cmp_op
        | 0xE0 -> step_imed_inst cpu Instruction.cpx_op
        | 0xE9 -> step_imed_inst cpu Instruction.sbc_op
        | _ -> cpu

    let step_indr (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with 0x6C -> step_indr_inst cpu Instruction.jmp_op | _ -> cpu

    let step_xind (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x01 -> step_xind_inst cpu Instruction.ora_op
        | 0x21 -> step_xind_inst cpu Instruction.and_op
        | 0x41 -> step_xind_inst cpu Instruction.eor_op
        | 0x61 -> step_xind_inst cpu Instruction.adc_op
        | 0x81 -> step_xind_inst cpu Instruction.sta_op
        | 0xA1 -> step_xind_inst cpu Instruction.lda_op
        | 0xC1 -> step_xind_inst cpu Instruction.cmp_op
        | 0xE1 -> step_xind_inst cpu Instruction.sbc_op
        | _ -> cpu

    let step_indy (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x11 -> step_indy_inst cpu Instruction.ora_op
        | 0x31 -> step_indy_inst cpu Instruction.and_op
        | 0x51 -> step_indy_inst cpu Instruction.eor_op
        | 0x71 -> step_indy_inst cpu Instruction.adc_op
        | 0x91 -> step_indy_inst cpu Instruction.sta_op
        | 0xB1 -> step_indy_inst cpu Instruction.lda_op
        | 0xD1 -> step_indy_inst cpu Instruction.cmp_op
        | 0xF1 -> step_indy_inst cpu Instruction.sbc_op
        | _ -> cpu

    let step_relt (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x10 -> step_relt_inst cpu Instruction.bpl_op
        | 0x30 -> step_relt_inst cpu Instruction.bmi_op
        | 0x50 -> step_relt_inst cpu Instruction.bvc_op
        | 0x70 -> step_relt_inst cpu Instruction.bvs_op
        | 0x90 -> step_relt_inst cpu Instruction.bcc_op
        | 0xB0 -> step_relt_inst cpu Instruction.bcs_op
        | 0xD0 -> step_relt_inst cpu Instruction.bne_op
        | 0xF0 -> step_relt_inst cpu Instruction.beq_op
        | _ -> cpu

    let step_zero (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x05 -> step_zero_inst cpu Instruction.ora_op
        | 0x06 -> step_zero_inst cpu Instruction.asl_op
        | 0x24 -> step_zero_inst cpu Instruction.bit_op
        | 0x25 -> step_zero_inst cpu Instruction.and_op
        | 0x26 -> step_zero_inst cpu Instruction.rol_op
        | 0x45 -> step_zero_inst cpu Instruction.eor_op
        | 0x46 -> step_zero_inst cpu Instruction.lsr_op
        | 0x65 -> step_zero_inst cpu Instruction.adc_op
        | 0x66 -> step_zero_inst cpu Instruction.ror_op
        | 0x84 -> step_zero_inst cpu Instruction.sty_op
        | 0x85 -> step_zero_inst cpu Instruction.sta_op
        | 0x86 -> step_zero_inst cpu Instruction.stx_op
        | 0xA4 -> step_zero_inst cpu Instruction.ldy_op
        | 0xA5 -> step_zero_inst cpu Instruction.lda_op
        | 0xA6 -> step_zero_inst cpu Instruction.ldx_op
        | 0xC4 -> step_zero_inst cpu Instruction.cpy_op
        | 0xC5 -> step_zero_inst cpu Instruction.cmp_op
        | 0xC6 -> step_zero_inst cpu Instruction.dec_op
        | 0xE4 -> step_zero_inst cpu Instruction.cpx_op
        | 0xE5 -> step_zero_inst cpu Instruction.sbc_op
        | 0xE6 -> step_zero_inst cpu Instruction.inc_op
        | _ -> cpu

    let step_zerx (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x15 -> step_zerx_inst cpu Instruction.ora_op
        | 0x16 -> step_zerx_inst cpu Instruction.asl_op
        | 0x35 -> step_zerx_inst cpu Instruction.and_op
        | 0x36 -> step_zerx_inst cpu Instruction.rol_op
        | 0x55 -> step_zerx_inst cpu Instruction.eor_op
        | 0x56 -> step_zerx_inst cpu Instruction.lsr_op
        | 0x75 -> step_zerx_inst cpu Instruction.adc_op
        | 0x76 -> step_zerx_inst cpu Instruction.ror_op
        | 0x94 -> step_zerx_inst cpu Instruction.sty_op
        | 0x95 -> step_zerx_inst cpu Instruction.sta_op
        | 0xB4 -> step_zerx_inst cpu Instruction.ldy_op
        | 0xB5 -> step_zerx_inst cpu Instruction.lda_op
        | 0xD5 -> step_zerx_inst cpu Instruction.cmp_op
        | 0xD6 -> step_zerx_inst cpu Instruction.dec_op
        | 0xF5 -> step_zerx_inst cpu Instruction.sbc_op
        | 0xF6 -> step_zerx_inst cpu Instruction.inc_op
        | _ -> cpu

    let step_zery (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x96 -> step_zery_inst cpu Instruction.stx_op
        | 0xB6 -> step_zery_inst cpu Instruction.ldx_op
        | _ -> cpu

    let step (cpu : CPU.t) (opcode : uint8) : CPU.t =
        let opcode = UInt8.to_int opcode in
        Decode.incr_cpu_pc cpu 1 |> step_none opcode |> step_accm opcode
        |> step_abst opcode |> step_absx opcode |> step_absy opcode
        |> step_imed opcode |> step_indr opcode |> step_xind opcode
        |> step_indy opcode |> step_relt opcode |> step_zero opcode
        |> step_zerx opcode |> step_zery opcode
end
