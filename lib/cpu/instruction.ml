open UInt8
open UInt16
open Cpu
open Decode

module Instruction = struct
    (* TODO: Finish implementing this function. *)
    let adc_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t =
        let operand = Decode.contents cpu mode in
        let summed_acc = cpu.accumulator ++ operand in
        let carry = ?>operand summed_acc in
        let neg_bit = ?-summed_acc in
        {
            cpu with
            accumulator = summed_acc;
            flags = { cpu.flags with carr_bit = carry; negative = neg_bit };
        }

    (* TODO: Finish implementing this function. *)
    let and_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implementing this function. *)
    let asl_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implementing this function. *)
    let bcc_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let bcs_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let beq_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let bit_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let bmi_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let bne_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let bpl_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let brk_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let bvc_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let bvs_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let clc_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let cld_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let cli_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let clv_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let cmp_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let cpx_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let cpy_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let dec_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let dex_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let dey_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let eor_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let inc_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let inx_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let iny_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let jmp_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let jsr_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let lda_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let ldx_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let ldy_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let lsr_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let nop_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let ora_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let pha_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let php_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let pla_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let plp_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let rol_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let ror_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let rti_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let rts_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let sbc_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let sec_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let sed_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let sei_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let sta_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let stx_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let sty_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let tax_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let tay_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let tsx_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let txa_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let txs_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu

    (* TODO: Finish implemetning this function. *)
    let tya_op (cpu : CPU.t) (type a) (mode : a Decode.memory_mode) : CPU.t = cpu
end
