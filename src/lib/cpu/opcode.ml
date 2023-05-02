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
        let operand = Decode.fetch_uint16_op cpu in
        let step_cpu = Decode.incr_cpu_pc cpu 2 in
        inst (Absolute operand) step_cpu

    let step_absx_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let operand = Decode.fetch_uint16_op cpu in
        let step_cpu = Decode.incr_cpu_pc cpu 2 in
        inst (AbsoluteX operand) step_cpu

    let step_absy_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let operand = Decode.fetch_uint16_op cpu in
        let step_cpu = Decode.incr_cpu_pc cpu 2 in
        inst (AbsoluteY operand) step_cpu

    let step_imed_inst (cpu : CPU.t) (inst : uint8 some_inst) : CPU.t =
        let operand = Decode.fetch_uint8_op cpu in
        let step_cpu = Decode.incr_cpu_pc cpu 1 in
        inst (Immediate operand) step_cpu

    let step_indr_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let operand = Decode.fetch_uint16_op cpu in
        let step_cpu = Decode.incr_cpu_pc cpu 2 in
        inst (Indirect operand) step_cpu

    let step_xind_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let operand = Decode.fetch_uint8_op cpu in
        let step_cpu = Decode.incr_cpu_pc cpu 1 in
        inst (XIndirect operand) step_cpu

    let step_indy_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let operand = Decode.fetch_uint8_op cpu in
        let step_cpu = Decode.incr_cpu_pc cpu 1 in
        inst (IndirectY operand) step_cpu

    let step_relt_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let operand = Decode.fetch_uint8_op cpu in
        let step_cpu = Decode.incr_cpu_pc cpu 1 in
        inst (Relative operand) step_cpu

    let step_zero_inst (cpu : CPU.t) (inst : uint8 some_inst) : CPU.t =
        let operand = Decode.fetch_uint8_op cpu in
        let step_cpu = Decode.incr_cpu_pc cpu 1 in
        inst (Zeropage operand) step_cpu

    let step_zerx_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let operand = Decode.fetch_uint8_op cpu in
        let step_cpu = Decode.incr_cpu_pc cpu 1 in
        inst (ZeropageX operand) step_cpu

    let step_zery_inst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        let operand = Decode.fetch_uint8_op cpu in
        let step_cpu = Decode.incr_cpu_pc cpu 1 in
        inst (ZeropageY operand) step_cpu

    let step_none (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x00 -> step_none_inst cpu Instruction.brk_op
        | 0x02 -> step_none_inst cpu Instruction.jam_op
        | 0x08 -> step_none_inst cpu Instruction.php_op
        | 0x12 -> step_none_inst cpu Instruction.jam_op
        | 0x18 -> step_none_inst cpu Instruction.clc_op
        | 0x1A -> step_none_inst cpu Instruction.nop_op
        | 0x22 -> step_none_inst cpu Instruction.jam_op
        | 0x28 -> step_none_inst cpu Instruction.plp_op
        | 0x32 -> step_none_inst cpu Instruction.jam_op
        | 0x38 -> step_none_inst cpu Instruction.sec_op
        | 0x3A -> step_none_inst cpu Instruction.nop_op
        | 0x40 -> step_none_inst cpu Instruction.rti_op
        | 0x42 -> step_none_inst cpu Instruction.jam_op
        | 0x48 -> step_none_inst cpu Instruction.pha_op
        | 0x52 -> step_none_inst cpu Instruction.jam_op
        | 0x58 -> step_none_inst cpu Instruction.cli_op
        | 0x5A -> step_none_inst cpu Instruction.nop_op
        | 0x60 -> step_none_inst cpu Instruction.rts_op
        | 0x62 -> step_none_inst cpu Instruction.jam_op
        | 0x68 -> step_none_inst cpu Instruction.pla_op
        | 0x72 -> step_none_inst cpu Instruction.jam_op
        | 0x78 -> step_none_inst cpu Instruction.sei_op
        | 0x7A -> step_none_inst cpu Instruction.nop_op
        | 0x88 -> step_none_inst cpu Instruction.dey_op
        | 0x8A -> step_none_inst cpu Instruction.txa_op
        | 0x92 -> step_none_inst cpu Instruction.jam_op
        | 0x98 -> step_none_inst cpu Instruction.tya_op
        | 0x9A -> step_none_inst cpu Instruction.txs_op
        | 0xA8 -> step_none_inst cpu Instruction.tay_op
        | 0xAA -> step_none_inst cpu Instruction.tax_op
        | 0xB2 -> step_none_inst cpu Instruction.jam_op
        | 0xB8 -> step_none_inst cpu Instruction.clv_op
        | 0xBA -> step_none_inst cpu Instruction.tsx_op
        | 0xC8 -> step_none_inst cpu Instruction.iny_op
        | 0xCA -> step_none_inst cpu Instruction.dex_op
        | 0xD2 -> step_none_inst cpu Instruction.jam_op
        | 0xD8 -> step_none_inst cpu Instruction.cld_op
        | 0xDA -> step_none_inst cpu Instruction.nop_op
        | 0xE8 -> step_none_inst cpu Instruction.inx_op
        | 0xEA -> step_none_inst cpu Instruction.nop_op
        | 0xF2 -> step_none_inst cpu Instruction.jam_op
        | 0xF8 -> step_none_inst cpu Instruction.sed_op
        | 0xFA -> step_none_inst cpu Instruction.nop_op
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
        | 0x0C -> step_abst_inst cpu Instruction.top_op
        | 0x0D -> step_abst_inst cpu Instruction.ora_op
        | 0x0E -> step_abst_inst cpu Instruction.asl_op
        | 0x0F -> step_abst_inst cpu Instruction.slo_op
        | 0x20 -> step_abst_inst cpu Instruction.jsr_op
        | 0x2C -> step_abst_inst cpu Instruction.bit_op
        | 0x2D -> step_abst_inst cpu Instruction.and_op
        | 0x2E -> step_abst_inst cpu Instruction.rol_op
        | 0x2F -> step_abst_inst cpu Instruction.rla_op
        | 0x4C -> step_abst_inst cpu Instruction.jmp_op
        | 0x4D -> step_abst_inst cpu Instruction.eor_op
        | 0x4E -> step_abst_inst cpu Instruction.lsr_op
        | 0x4F -> step_abst_inst cpu Instruction.sre_op
        | 0x6D -> step_abst_inst cpu Instruction.adc_op
        | 0x6E -> step_abst_inst cpu Instruction.ror_op
        | 0x6F -> step_abst_inst cpu Instruction.rra_op
        | 0x8C -> step_abst_inst cpu Instruction.sty_op
        | 0x8D -> step_abst_inst cpu Instruction.sta_op
        | 0x8E -> step_abst_inst cpu Instruction.stx_op
        | 0x8F -> step_abst_inst cpu Instruction.sax_op
        | 0xAC -> step_abst_inst cpu Instruction.ldy_op
        | 0xAD -> step_abst_inst cpu Instruction.lda_op
        | 0xAE -> step_abst_inst cpu Instruction.ldx_op
        | 0xAF -> step_abst_inst cpu Instruction.lax_op
        | 0xCC -> step_abst_inst cpu Instruction.cpy_op
        | 0xCD -> step_abst_inst cpu Instruction.cmp_op
        | 0xCE -> step_abst_inst cpu Instruction.dec_op
        | 0xCF -> step_abst_inst cpu Instruction.dcp_op
        | 0xEC -> step_abst_inst cpu Instruction.cpx_op
        | 0xED -> step_abst_inst cpu Instruction.sbc_op
        | 0xEE -> step_abst_inst cpu Instruction.inc_op
        | 0xEF -> step_abst_inst cpu Instruction.isc_op
        | _ -> cpu

    let step_absx (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x1C -> step_absx_inst cpu Instruction.top_op
        | 0x1D -> step_absx_inst cpu Instruction.ora_op
        | 0x1E -> step_absx_inst cpu Instruction.asl_op
        | 0x1F -> step_absx_inst cpu Instruction.slo_op
        | 0x3C -> step_absx_inst cpu Instruction.top_op
        | 0x3D -> step_absx_inst cpu Instruction.and_op
        | 0x3E -> step_absx_inst cpu Instruction.rol_op
        | 0x3F -> step_absx_inst cpu Instruction.rla_op
        | 0x5C -> step_absx_inst cpu Instruction.top_op
        | 0x5D -> step_absx_inst cpu Instruction.eor_op
        | 0x5E -> step_absx_inst cpu Instruction.lsr_op
        | 0x5F -> step_absx_inst cpu Instruction.sre_op
        | 0x7C -> step_absx_inst cpu Instruction.top_op
        | 0x7D -> step_absx_inst cpu Instruction.adc_op
        | 0x7E -> step_absx_inst cpu Instruction.ror_op
        | 0x7F -> step_absx_inst cpu Instruction.rra_op
        | 0x9D -> step_absx_inst cpu Instruction.sta_op
        | 0x9F -> step_absx_inst cpu Instruction.sha_op
        | 0xBC -> step_absx_inst cpu Instruction.ldy_op
        | 0xBD -> step_absx_inst cpu Instruction.lda_op
        | 0xDC -> step_absx_inst cpu Instruction.top_op
        | 0xDD -> step_absx_inst cpu Instruction.cmp_op
        | 0xDE -> step_absx_inst cpu Instruction.dec_op
        | 0xDF -> step_absx_inst cpu Instruction.dcp_op
        | 0xFC -> step_absx_inst cpu Instruction.top_op
        | 0xFD -> step_absx_inst cpu Instruction.sbc_op
        | 0xFE -> step_absx_inst cpu Instruction.inc_op
        | 0xFF -> step_absx_inst cpu Instruction.isc_op
        | _ -> cpu

    let step_absy (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x19 -> step_absy_inst cpu Instruction.ora_op
        | 0x1B -> step_absy_inst cpu Instruction.slo_op
        | 0x39 -> step_absy_inst cpu Instruction.and_op
        | 0x3B -> step_absy_inst cpu Instruction.rla_op
        | 0x59 -> step_absy_inst cpu Instruction.eor_op
        | 0x5B -> step_absy_inst cpu Instruction.sre_op
        | 0x79 -> step_absy_inst cpu Instruction.adc_op
        | 0x7B -> step_absy_inst cpu Instruction.rra_op
        | 0x99 -> step_absy_inst cpu Instruction.sta_op
        | 0x9F -> step_absy_inst cpu Instruction.sha_op
        | 0xBB -> step_absy_inst cpu Instruction.las_op
        | 0xB9 -> step_absy_inst cpu Instruction.lda_op
        | 0xBE -> step_absy_inst cpu Instruction.ldx_op
        | 0xBF -> step_absy_inst cpu Instruction.lax_op
        | 0xDB -> step_absy_inst cpu Instruction.dcp_op
        | 0xD9 -> step_absy_inst cpu Instruction.cmp_op
        | 0xF9 -> step_absy_inst cpu Instruction.sbc_op
        | 0xFB -> step_absy_inst cpu Instruction.isc_op
        | _ -> cpu

    let step_imed (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x09 -> step_imed_inst cpu Instruction.ora_op
        | 0x0B -> step_imed_inst cpu Instruction.anc_op
        | 0x29 -> step_imed_inst cpu Instruction.and_op
        | 0x2B -> step_imed_inst cpu Instruction.anc_op
        | 0x49 -> step_imed_inst cpu Instruction.eor_op
        | 0x4B -> step_imed_inst cpu Instruction.alr_op
        | 0x69 -> step_imed_inst cpu Instruction.adc_op
        | 0x80 -> step_imed_inst cpu Instruction.dop_op
        | 0x82 -> step_imed_inst cpu Instruction.dop_op
        | 0x89 -> step_imed_inst cpu Instruction.dop_op
        | 0x8B -> step_imed_inst cpu Instruction.ane_op
        | 0xA0 -> step_imed_inst cpu Instruction.ldy_op
        | 0xA2 -> step_imed_inst cpu Instruction.ldx_op
        | 0xA9 -> step_imed_inst cpu Instruction.lda_op
        | 0xAB -> step_imed_inst cpu Instruction.lxa_op
        | 0xC0 -> step_imed_inst cpu Instruction.cpy_op
        | 0xC2 -> step_imed_inst cpu Instruction.dop_op
        | 0xC9 -> step_imed_inst cpu Instruction.cmp_op
        | 0xCB -> step_imed_inst cpu Instruction.sbx_op
        | 0xE0 -> step_imed_inst cpu Instruction.cpx_op
        | 0xE2 -> step_imed_inst cpu Instruction.dop_op
        | 0xE9 -> step_imed_inst cpu Instruction.sbc_op
        | _ -> cpu

    let step_indr (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with 0x6C -> step_indr_inst cpu Instruction.jmp_op | _ -> cpu

    let step_xind (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x01 -> step_xind_inst cpu Instruction.ora_op
        | 0x03 -> step_xind_inst cpu Instruction.slo_op
        | 0x21 -> step_xind_inst cpu Instruction.and_op
        | 0x23 -> step_xind_inst cpu Instruction.rla_op
        | 0x41 -> step_xind_inst cpu Instruction.eor_op
        | 0x43 -> step_xind_inst cpu Instruction.sre_op
        | 0x61 -> step_xind_inst cpu Instruction.adc_op
        | 0x63 -> step_xind_inst cpu Instruction.rra_op
        | 0x81 -> step_xind_inst cpu Instruction.sta_op
        | 0x83 -> step_xind_inst cpu Instruction.sax_op
        | 0xA1 -> step_xind_inst cpu Instruction.lda_op
        | 0xA3 -> step_xind_inst cpu Instruction.lax_op
        | 0xC1 -> step_xind_inst cpu Instruction.cmp_op
        | 0xC3 -> step_xind_inst cpu Instruction.dcp_op
        | 0xE1 -> step_xind_inst cpu Instruction.sbc_op
        | 0xE3 -> step_xind_inst cpu Instruction.isc_op
        | _ -> cpu

    let step_indy (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x11 -> step_indy_inst cpu Instruction.ora_op
        | 0x13 -> step_indy_inst cpu Instruction.slo_op
        | 0x31 -> step_indy_inst cpu Instruction.and_op
        | 0x33 -> step_indy_inst cpu Instruction.rla_op
        | 0x51 -> step_indy_inst cpu Instruction.eor_op
        | 0x53 -> step_indy_inst cpu Instruction.sre_op
        | 0x71 -> step_indy_inst cpu Instruction.adc_op
        | 0x73 -> step_indy_inst cpu Instruction.rra_op
        | 0x91 -> step_indy_inst cpu Instruction.sta_op
        | 0x93 -> step_indy_inst cpu Instruction.sha_op
        | 0xB1 -> step_indy_inst cpu Instruction.lda_op
        | 0XB3 -> step_indy_inst cpu Instruction.lax_op
        | 0xD1 -> step_indy_inst cpu Instruction.cmp_op
        | 0xD3 -> step_indy_inst cpu Instruction.dcp_op
        | 0xF1 -> step_indy_inst cpu Instruction.sbc_op
        | 0xF3 -> step_indy_inst cpu Instruction.isc_op
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
        | 0x04 -> step_zero_inst cpu Instruction.dop_op
        | 0x05 -> step_zero_inst cpu Instruction.ora_op
        | 0x06 -> step_zero_inst cpu Instruction.asl_op
        | 0x07 -> step_zero_inst cpu Instruction.slo_op
        | 0x24 -> step_zero_inst cpu Instruction.bit_op
        | 0x25 -> step_zero_inst cpu Instruction.and_op
        | 0x26 -> step_zero_inst cpu Instruction.rol_op
        | 0x27 -> step_zero_inst cpu Instruction.rla_op
        | 0x44 -> step_zero_inst cpu Instruction.dop_op
        | 0x45 -> step_zero_inst cpu Instruction.eor_op
        | 0x47 -> step_zero_inst cpu Instruction.sre_op
        | 0x46 -> step_zero_inst cpu Instruction.lsr_op
        | 0x64 -> step_zero_inst cpu Instruction.dop_op
        | 0x65 -> step_zero_inst cpu Instruction.adc_op
        | 0x66 -> step_zero_inst cpu Instruction.ror_op
        | 0x67 -> step_zero_inst cpu Instruction.rra_op
        | 0x84 -> step_zero_inst cpu Instruction.sty_op
        | 0x85 -> step_zero_inst cpu Instruction.sta_op
        | 0x86 -> step_zero_inst cpu Instruction.stx_op
        | 0x87 -> step_zero_inst cpu Instruction.sax_op
        | 0xA4 -> step_zero_inst cpu Instruction.ldy_op
        | 0xA5 -> step_zero_inst cpu Instruction.lda_op
        | 0xA6 -> step_zero_inst cpu Instruction.ldx_op
        | 0xA7 -> step_zero_inst cpu Instruction.lax_op
        | 0xC4 -> step_zero_inst cpu Instruction.cpy_op
        | 0xC5 -> step_zero_inst cpu Instruction.cmp_op
        | 0xC6 -> step_zero_inst cpu Instruction.dec_op
        | 0xC7 -> step_zero_inst cpu Instruction.dcp_op
        | 0xE4 -> step_zero_inst cpu Instruction.cpx_op
        | 0xE5 -> step_zero_inst cpu Instruction.sbc_op
        | 0xE6 -> step_zero_inst cpu Instruction.inc_op
        | 0xE7 -> step_zero_inst cpu Instruction.isc_op
        | _ -> cpu

    let step_zerx (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x14 -> step_zerx_inst cpu Instruction.dop_op
        | 0x15 -> step_zerx_inst cpu Instruction.ora_op
        | 0x16 -> step_zerx_inst cpu Instruction.asl_op
        | 0x17 -> step_zerx_inst cpu Instruction.slo_op
        | 0x34 -> step_zerx_inst cpu Instruction.dop_op
        | 0x35 -> step_zerx_inst cpu Instruction.and_op
        | 0x36 -> step_zerx_inst cpu Instruction.rol_op
        | 0x37 -> step_zerx_inst cpu Instruction.rla_op
        | 0x54 -> step_zerx_inst cpu Instruction.dop_op
        | 0x55 -> step_zerx_inst cpu Instruction.eor_op
        | 0x56 -> step_zerx_inst cpu Instruction.lsr_op
        | 0x57 -> step_zerx_inst cpu Instruction.sre_op
        | 0x74 -> step_zerx_inst cpu Instruction.dop_op
        | 0x75 -> step_zerx_inst cpu Instruction.adc_op
        | 0x76 -> step_zerx_inst cpu Instruction.ror_op
        | 0x77 -> step_zerx_inst cpu Instruction.rra_op
        | 0x94 -> step_zerx_inst cpu Instruction.sty_op
        | 0x95 -> step_zerx_inst cpu Instruction.sta_op
        | 0xB4 -> step_zerx_inst cpu Instruction.ldy_op
        | 0xB5 -> step_zerx_inst cpu Instruction.lda_op
        | 0xD4 -> step_zerx_inst cpu Instruction.dop_op
        | 0xD5 -> step_zerx_inst cpu Instruction.cmp_op
        | 0xD6 -> step_zerx_inst cpu Instruction.dec_op
        | 0xD7 -> step_zerx_inst cpu Instruction.dcp_op
        | 0xF4 -> step_zerx_inst cpu Instruction.dop_op
        | 0xF5 -> step_zerx_inst cpu Instruction.sbc_op
        | 0xF6 -> step_zerx_inst cpu Instruction.inc_op
        | 0xF7 -> step_zerx_inst cpu Instruction.isc_op
        | _ -> cpu

    let step_zery (opcode : int) (cpu : CPU.t) : CPU.t =
        match opcode with
        | 0x96 -> step_zery_inst cpu Instruction.stx_op
        | 0x97 -> step_zery_inst cpu Instruction.sax_op
        | 0xB6 -> step_zery_inst cpu Instruction.ldx_op
        | 0xB7 -> step_zery_inst cpu Instruction.lax_op
        | _ -> cpu

    let step (cpu : CPU.t) (opcode : uint8) : CPU.t =
        let opcode = UInt8.to_int opcode in
        Decode.incr_cpu_pc cpu 1 |> step_none opcode |> step_accm opcode
        |> step_abst opcode |> step_absx opcode |> step_absy opcode
        |> step_imed opcode |> step_indr opcode |> step_xind opcode
        |> step_indy opcode |> step_relt opcode |> step_zero opcode
        |> step_zerx opcode |> step_zery opcode
end
