open UInt8
open Cpu

(** The Nintendo Entertainment System had over 150 different opcodes that
    updated the CPU state in different ways. However, the opcodes can be
    generalized into 56 different instructions that are implemented in
    [Instruction.Instruction]. This module provides functionality for matching
    each opcode to the instruction and memory mode it requires and updating
    the CPU as such. *)
module Opcode : sig
    val step : CPU.t -> uint8 -> CPU.t
    (** [step cpu op] performs exactly step of updating the CPU based on the
        opcode provided. *)
end
