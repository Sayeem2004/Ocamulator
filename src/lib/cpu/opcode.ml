open Cpu
open UInt8
open Decode
open UInt16
open Instruction

module Opcode = struct
    type none_inst = CPU.t -> CPU.t
    type 'a some_inst = 'a Decode.memory_mode -> CPU.t -> CPU.t

    let step_none (cpu : CPU.t) (inst : none_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 0 |> inst

    let step_accm (cpu : CPU.t) (inst : uint8 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 0 |> inst Accumulator

    let step_abst (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 2 |> inst (Absolute (Decode.fetch_uint16_op cpu))

    let step_absx (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 2 |> inst (AbsoluteX (Decode.fetch_uint16_op cpu))

    let step_absy (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 2 |> inst (AbsoluteY (Decode.fetch_uint16_op cpu))

    let step_imed (cpu : CPU.t) (inst : uint8 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 1 |> inst (Immediate (Decode.fetch_uint8_op cpu))

    let step_indr (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 2 |> inst (Indirect (Decode.fetch_uint16_op cpu))

    let step_xind (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 1 |> inst (XIndirect (Decode.fetch_uint8_op cpu))

    let step_indy (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 1 |> inst (IndirectY (Decode.fetch_uint8_op cpu))

    let step_relt (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 1 |> inst (Relative (Decode.fetch_uint8_op cpu))

    let step_zero (cpu : CPU.t) (inst : uint8 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 1 |> inst (Zeropage (Decode.fetch_uint8_op cpu))

    let step_zerx (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 1 |> inst (ZeropageX (Decode.fetch_uint8_op cpu))

    let step_zery (cpu : CPU.t) (inst : uint16 some_inst) : CPU.t =
        Decode.incr_cpu_pc cpu 1 |> inst (ZeropageY (Decode.fetch_uint8_op cpu))

    let step (cpu : CPU.t) (mode : uint8) : CPU.t =
        match UInt8.to_int mode with
        | 0x00 -> step_none cpu Instruction.brk_op
        | 0x01 -> step_xind cpu Instruction.ora_op
        | 0x05 -> step_zero cpu Instruction.ora_op
        | 0x06 -> step_zero cpu Instruction.asl_op
        | 0x08 -> step_none cpu Instruction.php_op
        | 0x09 -> step_imed cpu Instruction.ora_op
        | 0x0A -> step_accm cpu Instruction.asl_op
        | 0x0D -> step_abst cpu Instruction.ora_op
        | 0x0E -> step_abst cpu Instruction.asl_op
        | 0x10 -> step_relt cpu Instruction.bpl_op
        | 0x11 -> step_indy cpu Instruction.ora_op
        | 0x15 -> step_zerx cpu Instruction.ora_op
        | 0x16 -> step_zerx cpu Instruction.asl_op
        | 0x18 -> step_none cpu Instruction.clc_op
        | 0x19 -> step_absy cpu Instruction.ora_op
        | 0x1D -> step_absx cpu Instruction.ora_op
        | 0x1E -> step_absx cpu Instruction.asl_op
        | 0x20 -> step_abst cpu Instruction.jsr_op
        | 0x21 -> step_xind cpu Instruction.and_op
        | 0x24 -> step_zero cpu Instruction.bit_op
        | 0x25 -> step_zero cpu Instruction.and_op
        | 0x26 -> step_zero cpu Instruction.rol_op
        | 0x28 -> step_none cpu Instruction.plp_op
        | 0x29 -> step_imed cpu Instruction.and_op
        | 0x2A -> step_accm cpu Instruction.rol_op
        | 0x2C -> step_abst cpu Instruction.bit_op
        | 0x2D -> step_abst cpu Instruction.and_op
        | 0x2E -> step_abst cpu Instruction.rol_op
        | 0x30 -> step_relt cpu Instruction.bmi_op
        | 0x31 -> step_indy cpu Instruction.and_op
        | 0x35 -> step_zerx cpu Instruction.and_op
        | 0x36 -> step_zerx cpu Instruction.rol_op
        | 0x38 -> step_none cpu Instruction.sec_op
        | 0x39 -> step_absy cpu Instruction.and_op
        | 0x3D -> step_absx cpu Instruction.and_op
        | 0x3E -> step_absx cpu Instruction.rol_op
        | 0x40 -> step_none cpu Instruction.rti_op
        | 0x41 -> step_xind cpu Instruction.eor_op
        | 0x45 -> step_zero cpu Instruction.eor_op
        | 0x46 -> step_zero cpu Instruction.lsr_op
        | 0x48 -> step_none cpu Instruction.pha_op
        | 0x49 -> step_imed cpu Instruction.eor_op
        | 0x4A -> step_accm cpu Instruction.lsr_op
        | 0x4C -> step_abst cpu Instruction.jmp_op
        | 0x4D -> step_abst cpu Instruction.eor_op
        | 0x4E -> step_abst cpu Instruction.lsr_op
        | 0x50 -> step_relt cpu Instruction.bvc_op
        | 0x51 -> step_indy cpu Instruction.eor_op
        | 0x55 -> step_zerx cpu Instruction.eor_op
        | 0x56 -> step_zerx cpu Instruction.lsr_op
        | 0x58 -> step_none cpu Instruction.cli_op
        | 0x59 -> step_absy cpu Instruction.eor_op
        | 0x5D -> step_absx cpu Instruction.eor_op
        | 0x5E -> step_absx cpu Instruction.lsr_op
        | 0x60 -> step_none cpu Instruction.rts_op
        | 0x61 -> step_xind cpu Instruction.adc_op
        | 0x65 -> step_zero cpu Instruction.adc_op
        | 0x66 -> step_zero cpu Instruction.ror_op
        | 0x68 -> step_none cpu Instruction.pla_op
        | 0x69 -> step_imed cpu Instruction.adc_op
        | 0x6A -> step_accm cpu Instruction.ror_op
        | 0x6C -> step_indr cpu Instruction.jmp_op
        | 0x6D -> step_abst cpu Instruction.adc_op
        | 0x6E -> step_abst cpu Instruction.ror_op
        | 0x70 -> step_relt cpu Instruction.bvs_op
        | 0x71 -> step_indy cpu Instruction.adc_op
        | 0x75 -> step_zerx cpu Instruction.adc_op
        | 0x76 -> step_zerx cpu Instruction.ror_op
        | 0x78 -> step_none cpu Instruction.sei_op
        | 0x79 -> step_absy cpu Instruction.adc_op
        | 0x7D -> step_absx cpu Instruction.adc_op
        | 0x7E -> step_absx cpu Instruction.ror_op
        | 0x81 -> step_xind cpu Instruction.sta_op
        | 0x84 -> step_zero cpu Instruction.sty_op
        | 0x85 -> step_zero cpu Instruction.sta_op
        | 0x86 -> step_zero cpu Instruction.stx_op
        | 0x88 -> step_none cpu Instruction.dey_op
        | 0x8A -> step_none cpu Instruction.txa_op
        | 0x8C -> step_abst cpu Instruction.sty_op
        | 0x8D -> step_abst cpu Instruction.sta_op
        | 0x8E -> step_abst cpu Instruction.stx_op
        | 0x90 -> step_relt cpu Instruction.bcc_op
        | 0x91 -> step_indy cpu Instruction.sta_op
        | 0x94 -> step_zerx cpu Instruction.sty_op
        | 0x95 -> step_zerx cpu Instruction.sta_op
        | 0x96 -> step_zery cpu Instruction.stx_op
        | 0x98 -> step_none cpu Instruction.tya_op
        | 0x99 -> step_absy cpu Instruction.sta_op
        | 0x9A -> step_none cpu Instruction.txs_op
        | 0x9D -> step_absx cpu Instruction.sta_op
        | 0xA0 -> step_imed cpu Instruction.ldy_op
        | 0xA1 -> step_xind cpu Instruction.lda_op
        | 0xA2 -> step_imed cpu Instruction.ldx_op
        | 0xA4 -> step_zero cpu Instruction.ldy_op
        | 0xA5 -> step_zero cpu Instruction.lda_op
        | 0xA6 -> step_zero cpu Instruction.ldx_op
        | 0xA8 -> step_none cpu Instruction.tay_op
        | 0xA9 -> step_imed cpu Instruction.lda_op
        | 0xAA -> step_none cpu Instruction.tax_op
        | 0xAC -> step_abst cpu Instruction.ldy_op
        | 0xAD -> step_abst cpu Instruction.lda_op
        | 0xAE -> step_abst cpu Instruction.ldx_op
        | 0xB0 -> step_relt cpu Instruction.bcs_op
        | 0xB1 -> step_indy cpu Instruction.lda_op
        | 0xB4 -> step_zerx cpu Instruction.ldy_op
        | 0xB5 -> step_zerx cpu Instruction.lda_op
        | 0xB6 -> step_zery cpu Instruction.ldx_op
        | 0xB8 -> step_none cpu Instruction.clv_op
        | 0xB9 -> step_absy cpu Instruction.lda_op
        | 0xBA -> step_none cpu Instruction.tsx_op
        | 0xBC -> step_absx cpu Instruction.ldy_op
        | 0xBD -> step_absx cpu Instruction.lda_op
        | 0xBE -> step_absy cpu Instruction.ldx_op
        | 0xC0 -> step_imed cpu Instruction.cpy_op
        | 0xC1 -> step_xind cpu Instruction.cmp_op
        | 0xC4 -> step_zero cpu Instruction.cpy_op
        | 0xC5 -> step_zero cpu Instruction.cmp_op
        | 0xC6 -> step_zero cpu Instruction.dec_op
        | 0xC8 -> step_none cpu Instruction.iny_op
        | 0xC9 -> step_imed cpu Instruction.cmp_op
        | 0xCA -> step_none cpu Instruction.dex_op
        | 0xCC -> step_abst cpu Instruction.cpy_op
        | 0xCD -> step_abst cpu Instruction.cmp_op
        | 0xCE -> step_abst cpu Instruction.dec_op
        | 0xD0 -> step_relt cpu Instruction.bne_op
        | 0xD1 -> step_indy cpu Instruction.cmp_op
        | 0xD5 -> step_zerx cpu Instruction.cmp_op
        | 0xD6 -> step_zerx cpu Instruction.dec_op
        | 0xD8 -> step_none cpu Instruction.cld_op
        | 0xD9 -> step_absy cpu Instruction.cmp_op
        | 0xDD -> step_absx cpu Instruction.cmp_op
        | 0xDE -> step_absx cpu Instruction.dec_op
        | 0xE0 -> step_imed cpu Instruction.cpx_op
        | 0xE1 -> step_xind cpu Instruction.sbc_op
        | 0xE4 -> step_zero cpu Instruction.cpx_op
        | 0xE5 -> step_zero cpu Instruction.sbc_op
        | 0xE6 -> step_zero cpu Instruction.inc_op
        | 0xE8 -> step_none cpu Instruction.inx_op
        | 0xE9 -> step_imed cpu Instruction.sbc_op
        | 0xEA -> step_none cpu Instruction.nop_op
        | 0xEC -> step_abst cpu Instruction.cpx_op
        | 0xED -> step_abst cpu Instruction.sbc_op
        | 0xEE -> step_abst cpu Instruction.inc_op
        | 0xF0 -> step_relt cpu Instruction.beq_op
        | 0xF1 -> step_indy cpu Instruction.sbc_op
        | 0xF5 -> step_zerx cpu Instruction.sbc_op
        | 0xF6 -> step_zerx cpu Instruction.inc_op
        | 0xF8 -> step_none cpu Instruction.sed_op
        | 0xF9 -> step_absy cpu Instruction.sbc_op
        | 0xFD -> step_absx cpu Instruction.sbc_op
        | 0xFE -> step_absx cpu Instruction.inc_op
        | _ -> cpu
end
