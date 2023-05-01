open Cpu
open Decode

(** The Nintendo Entertainment System had 56 different instructions that
    updated the CPU state in over 150 different ways. [Instruction.Instruction]
    is a module that contains the implementation of these 56 different
    instructions. *)
module Instruction : sig
    val adc_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [adc_op] implements the ADC instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC *)

    val and_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [and_op] implements the AND instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#AND *)

    val asl_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [asl_op] implements the ASL instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#ASL *)

    val bcc_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [bcc_op] implements the BCC instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#BCC *)

    val bcs_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [bcs_op] implements the BCS instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#BCS *)

    val beq_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [beq_op] implements the BEQ instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#BEQ *)

    val bit_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [bit_op] implements the BIT instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#BIT *)

    val bmi_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [bmi_op] implements the BMI instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#BMI *)

    val bne_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [bne_op] implements the BNE instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#BNE *)

    val bpl_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [bpl_op] implements the BPL instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#BPL *)

    val brk_op : CPU.t -> CPU.t
    (** [brk_op] implements the BRK instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#BRK *)

    val bvc_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [bvc_op] implements the BVC instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#BVC *)

    val bvs_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [bvs_op] implements the BVS instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#BVS *)

    val clc_op : CPU.t -> CPU.t
    (** [clc_op] implements the CLC instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#CLC *)

    val cld_op : CPU.t -> CPU.t
    (** [cld_op] implements the CLD instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#CLD *)

    val cli_op : CPU.t -> CPU.t
    (** [cli_op] implements the CLI instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#CLI *)

    val clv_op : CPU.t -> CPU.t
    (** [clv_op] implements the CLV instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#CLV *)

    val cmp_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [cmp_op] implements the CMP instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#CMP *)

    val cpx_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [cpx_op] implements the CPX instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#CPX *)

    val cpy_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [cpy_op] implements the CPY instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#CPY *)

    val dec_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [dec_op] implements the DEC instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#DEC *)

    val dex_op : CPU.t -> CPU.t
    (** [dex_op] implements the DEX instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#DEX *)

    val dey_op : CPU.t -> CPU.t
    (** [dey_op] implements the DEY instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#DEY *)

    val eor_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [eor_op] implements the EOR instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#EOR *)

    val inc_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [inc_op] implements the INC instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#INC *)

    val inx_op : CPU.t -> CPU.t
    (** [inx_op] implements the INX instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#INX *)

    val iny_op : CPU.t -> CPU.t
    (** [iny_op] implements the INY instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#INY *)

    val jmp_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [jmp_op] implements the JMP instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP *)

    val jsr_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [jsr_op] implements the JSR instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#JSR *)

    val lda_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [lda_op] implements the LDA instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#LDA *)

    val ldx_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [ldx_op] implements the LDX instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#LDX *)

    val ldy_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [ldy_op] implements the LDY instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#LDY *)

    val lsr_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [lsr_op] implements the LSR instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#LSR *)

    val nop_op : CPU.t -> CPU.t
    (** [nop_op] implements the NOP instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#NOP *)

    val ora_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [ora_op] implements the ORA instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#ORA *)

    val pha_op : CPU.t -> CPU.t
    (** [pha_op] implements the PHA instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#PHA *)

    val php_op : CPU.t -> CPU.t
    (** [php_op] implements the PHP instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#PHP *)

    val pla_op : CPU.t -> CPU.t
    (** [pla_op] implements the PLA instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#PLA *)

    val plp_op : CPU.t -> CPU.t
    (** [plp_op] implements the PLP instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#PLP *)

    val rol_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [rol_op] implements the ROL instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#ROL *)

    val ror_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [ror_op] implements the ROR instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#ROR *)

    val rti_op : CPU.t -> CPU.t
    (** [rti_op] implements the RTI instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#RTI *)

    val rts_op : CPU.t -> CPU.t
    (** [rts_op] implements the RTS instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#RTS *)

    val sbc_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [sbc_op] implements the SBC instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC *)

    val sec_op : CPU.t -> CPU.t
    (** [sec_op] implements the SEC instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#SEC *)

    val sed_op : CPU.t -> CPU.t
    (** [sed_op] implements the SED instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#SED *)

    val sei_op : CPU.t -> CPU.t
    (** [sei_op] implements the SEI instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#SEI *)

    val sta_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [sta_op] implements the STA instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#STA *)

    val stx_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [stx_op] implements the STX instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#STX *)

    val sty_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [sty_op] implements the STY instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#STY *)

    val tax_op : CPU.t -> CPU.t
    (** [tax_op] implements the TAX instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#TAX *)

    val tay_op : CPU.t -> CPU.t
    (** [tay_op] implements the TAY instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#TAY *)

    val tsx_op : CPU.t -> CPU.t
    (** [tsx_op] implements the TSX instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#TSX *)

    val txa_op : CPU.t -> CPU.t
    (** [txa_op] implements the TXA instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#TXA *)

    val txs_op : CPU.t -> CPU.t
    (** [txs_op] implements the TXS instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#TXS *)

    val tya_op : CPU.t -> CPU.t
    (** [tya_op] implements the TYA instruction found here:
        https://www.nesdev.org/obelisk-6502-guide/reference.html#TYA *)

    val dop_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [dop_op] implements the DOP instruction found here:
        https://www.masswerk.at/6502/6502_instruction_set.html#DOP *)

    val top_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [top_op] implements the TOP instruction found here:
        https://www.masswerk.at/6502/6502_instruction_set.html#TOP *)

    val alr_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [alr_op] implements the ALR instruction found here:
        https://www.masswerk.at/6502/6502_instruction_set.html#ALR *)

    val anc_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [anc_op] implements the ANC instruction found here:
        https://www.masswerk.at/6502/6502_instruction_set.html#ANC *)

    val sax_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [sax_op] implements the SAX instruction found here:
        https://www.masswerk.at/6502/6502_instruction_set.html#SAX *)

    val ane_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [ane_op] implements the ANE instruction found here:
        https://www.masswerk.at/6502/6502_instruction_set.html#ANE *)

    val lax_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [lax_op] implements the LAX instruction found here:
        https://www.masswerk.at/6502/6502_instruction_set.html#LAX *)

    val las_op : 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [las_op] implements the LAS instruction found here:
        https://www.masswerk.at/6502/6502_instruction_set.html#LAS *)
end
