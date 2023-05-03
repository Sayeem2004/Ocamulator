open Alias

(** [Instruction.ml] contains the ~ 80 instructions used by the emulator. *)

val adc_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [adc_op] implements the ADC instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#ADC *)

val and_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [and_op] implements the AND instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#AND *)

val asl_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [asl_op] implements the ASL instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#ASL *)

val bcc_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [bcc_op] implements the BCC instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#BCC *)

val bcs_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [bcs_op] implements the BCS instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#BCS *)

val beq_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [beq_op] implements the BEQ instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#BEQ *)

val bit_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [bit_op] implements the BIT instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#BIT *)

val bmi_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [bmi_op] implements the BMI instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#BMI *)

val bne_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [bne_op] implements the BNE instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#BNE *)

val bpl_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [bpl_op] implements the BPL instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#BPL *)

val brk_op : Cpu.t -> Cpu.t
(** [brk_op] implements the BRK instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#BRK *)

val bvc_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [bvc_op] implements the BVC instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#BVC *)

val bvs_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [bvs_op] implements the BVS instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#BVS *)

val clc_op : Cpu.t -> Cpu.t
(** [clc_op] implements the CLC instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#CLC *)

val cld_op : Cpu.t -> Cpu.t
(** [cld_op] implements the CLD instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#CLD *)

val cli_op : Cpu.t -> Cpu.t
(** [cli_op] implements the CLI instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#CLI *)

val clv_op : Cpu.t -> Cpu.t
(** [clv_op] implements the CLV instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#CLV *)

val cmp_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [cmp_op] implements the CMP instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#CMP *)

val cpx_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [cpx_op] implements the CPX instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#CPX *)

val cpy_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [cpy_op] implements the CPY instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#CPY *)

val dec_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [dec_op] implements the DEC instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#DEC *)

val dex_op : Cpu.t -> Cpu.t
(** [dex_op] implements the DEX instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#DEX *)

val dey_op : Cpu.t -> Cpu.t
(** [dey_op] implements the DEY instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#DEY *)

val eor_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [eor_op] implements the EOR instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#EOR *)

val inc_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [inc_op] implements the INC instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#INC *)

val inx_op : Cpu.t -> Cpu.t
(** [inx_op] implements the INX instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#INX *)

val iny_op : Cpu.t -> Cpu.t
(** [iny_op] implements the INY instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#INY *)

val jmp_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [jmp_op] implements the JMP instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#JMP *)

val jsr_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [jsr_op] implements the JSR instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#JSR *)

val lda_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [lda_op] implements the LDA instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#LDA *)

val ldx_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [ldx_op] implements the LDX instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#LDX *)

val ldy_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [ldy_op] implements the LDY instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#LDY *)

val lsr_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [lsr_op] implements the LSR instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#LSR *)

val nop_op : Cpu.t -> Cpu.t
(** [nop_op] implements the NOP instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#NOP *)

val ora_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [ora_op] implements the ORA instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#ORA *)

val pha_op : Cpu.t -> Cpu.t
(** [pha_op] implements the PHA instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#PHA *)

val php_op : Cpu.t -> Cpu.t
(** [php_op] implements the PHP instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#PHP *)

val pla_op : Cpu.t -> Cpu.t
(** [pla_op] implements the PLA instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#PLA *)

val plp_op : Cpu.t -> Cpu.t
(** [plp_op] implements the PLP instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#PLP *)

val rol_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [rol_op] implements the ROL instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#ROL *)

val ror_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [ror_op] implements the ROR instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#ROR *)

val rti_op : Cpu.t -> Cpu.t
(** [rti_op] implements the RTI instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#RTI *)

val rts_op : Cpu.t -> Cpu.t
(** [rts_op] implements the RTS instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#RTS *)

val sbc_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [sbc_op] implements the SBC instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#SBC *)

val sec_op : Cpu.t -> Cpu.t
(** [sec_op] implements the SEC instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#SEC *)

val sed_op : Cpu.t -> Cpu.t
(** [sed_op] implements the SED instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#SED *)

val sei_op : Cpu.t -> Cpu.t
(** [sei_op] implements the SEI instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#SEI *)

val sta_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [sta_op] implements the STA instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#STA *)

val stx_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [stx_op] implements the STX instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#STX *)

val sty_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [sty_op] implements the STY instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#STY *)

val tax_op : Cpu.t -> Cpu.t
(** [tax_op] implements the TAX instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#TAX *)

val tay_op : Cpu.t -> Cpu.t
(** [tay_op] implements the TAY instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#TAY *)

val tsx_op : Cpu.t -> Cpu.t
(** [tsx_op] implements the TSX instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#TSX *)

val txa_op : Cpu.t -> Cpu.t
(** [txa_op] implements the TXA instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#TXA *)

val txs_op : Cpu.t -> Cpu.t
(** [txs_op] implements the TXS instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#TXS *)

val tya_op : Cpu.t -> Cpu.t
(** [tya_op] implements the TYA instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#TYA *)

val dop_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [dop_op] implements the DOP instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#DOP *)

val top_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [top_op] implements the TOP instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#TOP *)

val alr_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [alr_op] implements the ALR instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#ALR *)

val anc_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [anc_op] implements the ANC instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#ANC *)

val sax_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [sax_op] implements the SAX instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#SAX *)

val ane_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [ane_op] implements the ANE instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#ANE *)

val lax_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [lax_op] implements the LAX instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#LAX *)

val las_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [las_op] implements the LAS instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#LAS *)

val dcp_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [dcp_op] implements the DCP instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#DCP *)

val lxa_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [lxa_op] implements the LXA instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#LXA *)

val isc_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [isc_op] implements the ISC instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#ISC *)

val rla_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [rla_op] implements the RLA instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#RLA *)

val slo_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [slo_op] implements the SLO instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#SLO *)

val sre_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [sre_op] implements the SRE instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#SRE *)

val jam_op : Cpu.t -> Cpu.t
(** [jam_op] implements the JAM instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#JAM *)

val rra_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [rra_op] implements the RRA instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#RRA *)

val sbx_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [sbx_op] implements the SBX instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#SBX *)

val sha_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [sha_op] implements the SHA instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#SHA *)

val tas_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [tas_op] implements the TAS instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#TAS *)

val shy_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [shy_op] implements the SHY instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#SHY *)

val shx_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [shx_op] implements the SHX instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#SHX *)

val arr_op : 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [arr_op] implements the ARR instruction found here:
    https://www.masswerk.at/6502/6502_instruction_set.html#ARR *)
