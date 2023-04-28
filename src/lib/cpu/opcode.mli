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

    val step_none_inst : CPU.t -> none_inst -> CPU.t
    (** [step_none_inst cpu inst] performs exactly one step of updating the CPU using
        none memory mode and the instruction provided. *)

    val step_accm_inst : CPU.t -> uint8 some_inst -> CPU.t
    (** [step_accm_inst cpu inst] performs exactly one step of updating the CPU using
        accumulator memory mode and the instruction provided. *)

    val step_abst_inst : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_abst_inst cpu inst] performs exactly one step of updating the CPU using
        absolute memory mode and the instruction provided. *)

    val step_absx_inst : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_absx_inst cpu inst] performs exactly one step of updating the CPU using
        absolute x memory mode and the instruction provided. *)

    val step_absy_inst : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_absy_inst cpu inst] performs exactly one step of updating the CPU using
        absolute y memory mode and the instruction provided. *)

    val step_imed_inst : CPU.t -> uint8 some_inst -> CPU.t
    (** [step_imed_inst cpu inst] performs exactly one step of updating the CPU using
        immediate memory mode and the instruction provided. *)

    val step_indr_inst : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_indr_inst cpu inst] performs exactly one step of updating the CPU using
        indirect memory mode and the instruction provided. *)

    val step_xind_inst : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_xind_inst cpu inst] performs exactly one step of updating the CPU using
        x indirect memory mode and the instruction provided. *)

    val step_indy_inst : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_indy_inst cpu inst] performs exactly one step of updating the CPU using
        indirect y memory mode and the instruction provided. *)

    val step_relt_inst : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_relt_inst cpu inst] performs exactly one step of updating the CPU using
        relative memory mode and the instruction provided. *)

    val step_zero_inst : CPU.t -> uint8 some_inst -> CPU.t
    (** [step_zero_inst cpu inst] performs exactly one step of updating the CPU using
        zero memory mode and the instruction provided. *)

    val step_zerx_inst : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_zerx_inst cpu inst] performs exactly one step of updating the CPU using
        zero x memory mode and the instruction provided. *)

    val step_zery_inst : CPU.t -> uint16 some_inst -> CPU.t
    (** [step_zery_inst cpu inst] performs exactly one step of updating the CPU using
        zero y memory mode and the instruction provided. *)

    val step_none : int -> CPU.t -> CPU.t
    (** [step_none mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the none memory mode and the matched instruction. *)

    val step_accm : int -> CPU.t -> CPU.t
    (** [step_accm mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the accumulator memory mode and the matched instruction. *)

    val step_abst : int -> CPU.t -> CPU.t
    (** [step_abst mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the absolute memory mode and the matched instruction. *)

    val step_absx : int -> CPU.t -> CPU.t
    (** [step_absx mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the absolute x memory mode and the matched instruction. *)

    val step_absy : int -> CPU.t -> CPU.t
    (** [step_absy mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the absolute y memory mode and the matched instruction. *)

    val step_imed : int -> CPU.t -> CPU.t
    (** [step_imed mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the immediate memory mode and the matched instruction. *)

    val step_indr : int -> CPU.t -> CPU.t
    (** [step_indr mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the indirect memory mode and the matched instruction. *)

    val step_xind : int -> CPU.t -> CPU.t
    (** [step_xind mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the x indirect memory mode and the matched instruction. *)

    val step_indy : int -> CPU.t -> CPU.t
    (** [step_indy mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the indirect y memory mode and the matched instruction. *)

    val step_relt : int -> CPU.t -> CPU.t
    (** [step_relt mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the relative memory mode and the matched instruction. *)

    val step_zero : int -> CPU.t -> CPU.t
    (** [step_zero mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the zero memory mode and the matched instruction. *)

    val step_zerx : int -> CPU.t -> CPU.t
    (** [step_zerx mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the zero x memory mode and the matched instruction. *)

    val step_zery : int -> CPU.t -> CPU.t
    (** [step_zery mode cpu] matches the mode to an instruction and performs one step
        of updating the CPU using the zero y memory mode and the matched instruction. *)

    val step : CPU.t -> uint8 -> CPU.t
    (** [step cpu mode] matches the mode to a memory mode and then performs one step
        of updating the CPU using the matched memory mode. *)
end
