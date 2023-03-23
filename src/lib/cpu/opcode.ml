open UInt8
open UInt16
open Cpu
open Decode
open Instruction

module Opcode = struct
    let step (cpu : CPU.t) (mode : uint8) : CPU.t =
        match UInt8.to_int mode with
        | 0x00 -> Decode.incr_cpu_pc cpu 0 |> Instruction.brk_op
        | 0x01 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ora_op (XIndirect (Decode.fetch_uint8_op cpu))
        | 0x05 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ora_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x06 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.asl_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x08 -> Decode.incr_cpu_pc cpu 0 |> Instruction.php_op
        | 0x09 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ora_op (Immediate (Decode.fetch_uint8_op cpu))
        | 0x0A -> Decode.incr_cpu_pc cpu 0 |> Instruction.asl_op Accumulator
        | 0x0D ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.ora_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x0E ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.asl_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x10 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.bpl_op (Relative (Decode.fetch_uint8_op cpu))
        | 0x11 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ora_op (IndirectY (Decode.fetch_uint8_op cpu))
        | 0x15 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ora_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0x16 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.asl_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0x18 -> Decode.incr_cpu_pc cpu 0 |> Instruction.clc_op
        | 0x19 ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.ora_op (AbsoluteY (Decode.fetch_uint16_op cpu))
        | 0x1D ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.ora_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0x1E ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.asl_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0x20 ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.jsr_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x21 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.and_op (XIndirect (Decode.fetch_uint8_op cpu))
        | 0x24 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.bit_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x25 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.and_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x26 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.rol_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x28 -> Decode.incr_cpu_pc cpu 0 |> Instruction.plp_op
        | 0x29 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.and_op (Immediate (Decode.fetch_uint8_op cpu))
        | 0x2A -> Decode.incr_cpu_pc cpu 0 |> Instruction.rol_op Accumulator
        | 0x2C ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.bit_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x2D ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.and_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x2E ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.rol_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x30 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.bmi_op (Relative (Decode.fetch_uint8_op cpu))
        | 0x31 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.and_op (IndirectY (Decode.fetch_uint8_op cpu))
        | 0x35 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.and_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0x36 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.rol_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0x38 -> Decode.incr_cpu_pc cpu 0 |> Instruction.sec_op
        | 0x39 ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.and_op (AbsoluteY (Decode.fetch_uint16_op cpu))
        | 0x3D ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.and_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0x3E ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.rol_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0x40 -> Decode.incr_cpu_pc cpu 0 |> Instruction.rti_op
        | 0x41 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.eor_op (XIndirect (Decode.fetch_uint8_op cpu))
        | 0x45 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.eor_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x46 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.lsr_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x48 -> Decode.incr_cpu_pc cpu 0 |> Instruction.pha_op
        | 0x49 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.eor_op (Immediate (Decode.fetch_uint8_op cpu))
        | 0x4A -> Decode.incr_cpu_pc cpu 0 |> Instruction.lsr_op Accumulator
        | 0x4C ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.jmp_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x4D ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.eor_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x4E ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.lsr_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x50 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.bvc_op (Relative (Decode.fetch_uint8_op cpu))
        | 0x51 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.eor_op (IndirectY (Decode.fetch_uint8_op cpu))
        | 0x55 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.eor_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0x56 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.lsr_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0x58 -> Decode.incr_cpu_pc cpu 0 |> Instruction.cli_op
        | 0x59 ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.eor_op (AbsoluteY (Decode.fetch_uint16_op cpu))
        | 0x5D ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.eor_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0x5E ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.lsr_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0x60 -> Decode.incr_cpu_pc cpu 0 |> Instruction.rts_op
        | 0x61 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.adc_op (XIndirect (Decode.fetch_uint8_op cpu))
        | 0x65 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.adc_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x66 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ror_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x68 -> Decode.incr_cpu_pc cpu 0 |> Instruction.pla_op
        | 0x69 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.adc_op (Immediate (Decode.fetch_uint8_op cpu))
        | 0x6A -> Decode.incr_cpu_pc cpu 0 |> Instruction.ror_op Accumulator
        | 0x6C ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.jmp_op (Indirect (Decode.fetch_uint16_op cpu))
        | 0x6D ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.adc_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x6E ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.ror_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x70 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.bvs_op (Relative (Decode.fetch_uint8_op cpu))
        | 0x71 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.adc_op (IndirectY (Decode.fetch_uint8_op cpu))
        | 0x75 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.adc_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0x76 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ror_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0x78 -> Decode.incr_cpu_pc cpu 0 |> Instruction.sei_op
        | 0x79 ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.adc_op (AbsoluteY (Decode.fetch_uint16_op cpu))
        | 0x7D ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.adc_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0x7E ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.ror_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0x81 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.sta_op (XIndirect (Decode.fetch_uint8_op cpu))
        | 0x84 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.sty_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x85 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.sta_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x86 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.stx_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0x88 -> Decode.incr_cpu_pc cpu 0 |> Instruction.dey_op
        | 0x8A -> Decode.incr_cpu_pc cpu 0 |> Instruction.txa_op
        | 0x8C ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.sty_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x8D ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.sta_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x8E ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.stx_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0x90 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.bcc_op (Relative (Decode.fetch_uint8_op cpu))
        | 0x91 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.sta_op (IndirectY (Decode.fetch_uint8_op cpu))
        | 0x94 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.sty_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0x95 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.sta_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0x96 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.stx_op (ZeropageY (Decode.fetch_uint8_op cpu))
        | 0x98 -> Decode.incr_cpu_pc cpu 0 |> Instruction.tya_op
        | 0x99 ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.sta_op (AbsoluteY (Decode.fetch_uint16_op cpu))
        | 0x9A -> Decode.incr_cpu_pc cpu 0 |> Instruction.txs_op
        | 0x9D ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.sta_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0xA0 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ldy_op (Immediate (Decode.fetch_uint8_op cpu))
        | 0xA1 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.lda_op (XIndirect (Decode.fetch_uint8_op cpu))
        | 0xA2 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ldx_op (Immediate (Decode.fetch_uint8_op cpu))
        | 0xA4 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ldy_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0xA5 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.lda_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0xA6 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ldx_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0xA8 -> Decode.incr_cpu_pc cpu 0 |> Instruction.tay_op
        | 0xA9 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.lda_op (Immediate (Decode.fetch_uint8_op cpu))
        | 0xAA -> Decode.incr_cpu_pc cpu 0 |> Instruction.tax_op
        | 0xAC ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.ldy_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0xAD ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.lda_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0xAE ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.ldx_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0xB0 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.bcs_op (Relative (Decode.fetch_uint8_op cpu))
        | 0xB1 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.lda_op (IndirectY (Decode.fetch_uint8_op cpu))
        | 0xB4 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ldy_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0xB5 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.lda_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0xB6 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.ldx_op (ZeropageY (Decode.fetch_uint8_op cpu))
        | 0xB8 -> Decode.incr_cpu_pc cpu 0 |> Instruction.clv_op
        | 0xB9 ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.lda_op (AbsoluteY (Decode.fetch_uint16_op cpu))
        | 0xBA -> Decode.incr_cpu_pc cpu 0 |> Instruction.tsx_op
        | 0xBC ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.ldy_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0xBD ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.lda_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0xBE ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.ldx_op (AbsoluteY (Decode.fetch_uint16_op cpu))
        | 0xC0 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.cpy_op (Immediate (Decode.fetch_uint8_op cpu))
        | 0xC1 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.cmp_op (XIndirect (Decode.fetch_uint8_op cpu))
        | 0xC4 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.cpy_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0xC5 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.cmp_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0xC6 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.dec_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0xC8 -> Decode.incr_cpu_pc cpu 0 |> Instruction.iny_op
        | 0xC9 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.cmp_op (Immediate (Decode.fetch_uint8_op cpu))
        | 0xCA -> Decode.incr_cpu_pc cpu 0 |> Instruction.dex_op
        | 0xCC ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.cpy_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0xCD ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.cmp_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0xCE ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.dec_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0xD0 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.bne_op (Relative (Decode.fetch_uint8_op cpu))
        | 0xD1 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.cmp_op (IndirectY (Decode.fetch_uint8_op cpu))
        | 0xD5 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.cmp_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0xD6 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.dec_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0xD8 -> Decode.incr_cpu_pc cpu 0 |> Instruction.cld_op
        | 0xD9 ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.cmp_op (AbsoluteY (Decode.fetch_uint16_op cpu))
        | 0xDD ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.cmp_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0xDE ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.dec_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0xE0 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.cpx_op (Immediate (Decode.fetch_uint8_op cpu))
        | 0xE1 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.sbc_op (XIndirect (Decode.fetch_uint8_op cpu))
        | 0xE4 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.cpx_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0xE5 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.sbc_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0xE6 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.inc_op (Zeropage (Decode.fetch_uint8_op cpu))
        | 0xE8 -> Decode.incr_cpu_pc cpu 0 |> Instruction.inx_op
        | 0xE9 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.sbc_op (Immediate (Decode.fetch_uint8_op cpu))
        | 0xEA -> Decode.incr_cpu_pc cpu 0 |> Instruction.nop_op
        | 0xEC ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.cpx_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0xED ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.sbc_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0xEE ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.inc_op (Absolute (Decode.fetch_uint16_op cpu))
        | 0xF0 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.beq_op (Relative (Decode.fetch_uint8_op cpu))
        | 0xF1 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.sbc_op (IndirectY (Decode.fetch_uint8_op cpu))
        | 0xF5 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.sbc_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0xF6 ->
            Decode.incr_cpu_pc cpu 1
            |> Instruction.inc_op (ZeropageX (Decode.fetch_uint8_op cpu))
        | 0xF8 -> Decode.incr_cpu_pc cpu 0 |> Instruction.sed_op
        | 0xF9 ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.sbc_op (AbsoluteY (Decode.fetch_uint16_op cpu))
        | 0xFD ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.sbc_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | 0xFE ->
            Decode.incr_cpu_pc cpu 2
            |> Instruction.inc_op (AbsoluteX (Decode.fetch_uint16_op cpu))
        | _ -> cpu
        (* Todo: Change to this to failwith eventually. *)
end
