open Cpu
open UInt8
open Decode
open UInt16

(** The Nintendo Entertainment System had over 150 different opcodes that
    updated the CPU state in different ways. However, the opcodes can be
    generalized into 56 different instructions that are implemented in
    [Instruction.Instruction]. This module provides functionality for matching
    each opcode to the instruction and memory mode it requires and updating
    the CPU as such. *)
module Opcode : sig
    type none_inst = CPU.t -> CPU.t
    (** [none_func] is a function that takes a CPU and returns a CPU. *)

    type 'a some_inst = 'a Decode.memory_mode -> CPU.t -> CPU.t
    (** [some_func] is a function that takes a memory mode and a CPU and returns
        a CPU. *)

    val step_none : CPU.t -> none_inst -> CPU.t
    (** [step_none inst cpu] performs exactly one step of updating the CPU using
        no memory node and the instruction provided. *)

    val step_accm : CPU.t -> uint8 some_inst -> CPU.t
    (** [step_accm inst cpu] performs exactly one step of updating the CPU using
        the accumulator as the memory node and the instruction provided. *)

    val step_abst : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_abst inst cpu] performs exactly one step of updating the CPU using
        the absolute memory node and the instruction provided. *)

    val step_absx : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_absx inst cpu] performs exactly one step of updating the CPU using
        the absolute x memory node and the instruction provided. *)

    val step_absy : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_absy inst cpu] performs exactly one step of updating the CPU using
        the absolute y memory node and the instruction provided. *)

    val step_imed : CPU.t -> uint8 some_inst -> CPU.t
    (** [step_imed inst cpu] performs exactly one step of updating the CPU using
        the immediate memory node and the instruction provided. *)

    val step_indr : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_indr inst cpu] performs exactly one step of updating the CPU using
        the indirect memory node and the instruction provided. *)

    val step_xind : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_xind inst cpu] performs exactly one step of updating the CPU using
        the x indirect memory node and the instruction provided. *)

    val step_indy : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_indy inst cpu] performs exactly one step of updating the CPU using
        the indirect y memory node and the instruction provided. *)

    val step_relt : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_relt inst cpu] performs exactly one step of updating the CPU using
        the relative memory node and the instruction provided. *)

    val step_zero : CPU.t -> uint8 some_inst -> CPU.t
    (** [step_zero inst cpu] performs exactly one step of updating the CPU using
        the zero page memory node and the instruction provided. *)

    val step_zerx : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_zerx inst cpu] performs exactly one step of updating the CPU using
        the zero page x memory node and the instruction provided. *)

    val step_zery : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_zery inst cpu] performs exactly one step of updating the CPU using
        the zero page y memory node and the instruction provided. *)

    val step : CPU.t -> uint8 -> CPU.t
    (** [step cpu op] performs exactly step of updating the CPU based on the
        opcode provided in uint8 form. *)
end
