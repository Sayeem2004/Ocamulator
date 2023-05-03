open Alias

(** [Opcode.ml] matches an opcode to its instruction and memory mode. *)

type none_inst = Cpu.t -> Cpu.t
(** [none_func] is a function that takes a Cpu and returns a Cpu. *)

type 'a some_inst = 'a Decode.memory_mode -> Cpu.t -> Cpu.t
(** [some_func] is a function that takes a memory mode and a Cpu and returns
    a Cpu. *)

val step_none_inst : Cpu.t -> none_inst -> Cpu.t
(** [step_none_inst cpu inst] performs exactly one step of updating the Cpu using
    none memory mode and the instruction provided. *)

val step_accm_inst : Cpu.t -> uint8 some_inst -> Cpu.t
(** [step_accm_inst cpu inst] performs exactly one step of updating the Cpu using
    accumulator memory mode and the instruction provided. *)

val step_abst_inst : Cpu.t -> uint16 some_inst -> Cpu.t
(** [step_abst_inst cpu inst] performs exactly one step of updating the Cpu using
    absolute memory mode and the instruction provided. *)

val step_absx_inst : Cpu.t -> uint16 some_inst -> Cpu.t
(** [step_absx_inst cpu inst] performs exactly one step of updating the Cpu using
    absolute x memory mode and the instruction provided. *)

val step_absy_inst : Cpu.t -> uint16 some_inst -> Cpu.t
(** [step_absy_inst cpu inst] performs exactly one step of updating the Cpu using
    absolute y memory mode and the instruction provided. *)

val step_imed_inst : Cpu.t -> uint8 some_inst -> Cpu.t
(** [step_imed_inst cpu inst] performs exactly one step of updating the Cpu using
    immediate memory mode and the instruction provided. *)

val step_indr_inst : Cpu.t -> uint16 some_inst -> Cpu.t
(** [step_indr_inst cpu inst] performs exactly one step of updating the Cpu using
    indirect memory mode and the instruction provided. *)

val step_xind_inst : Cpu.t -> uint16 some_inst -> Cpu.t
(** [step_xind_inst cpu inst] performs exactly one step of updating the Cpu using
    x indirect memory mode and the instruction provided. *)

val step_indy_inst : Cpu.t -> uint16 some_inst -> Cpu.t
(** [step_indy_inst cpu inst] performs exactly one step of updating the Cpu using
    indirect y memory mode and the instruction provided. *)

val step_relt_inst : Cpu.t -> uint16 some_inst -> Cpu.t
(** [step_relt_inst cpu inst] performs exactly one step of updating the Cpu using
    relative memory mode and the instruction provided. *)

val step_zero_inst : Cpu.t -> uint8 some_inst -> Cpu.t
(** [step_zero_inst cpu inst] performs exactly one step of updating the Cpu using
    zero memory mode and the instruction provided. *)

val step_zerx_inst : Cpu.t -> uint16 some_inst -> Cpu.t
(** [step_zerx_inst cpu inst] performs exactly one step of updating the Cpu using
    zero x memory mode and the instruction provided. *)

val step_zery_inst : Cpu.t -> uint16 some_inst -> Cpu.t
(** [step_zery_inst cpu inst] performs exactly one step of updating the Cpu using
    zero y memory mode and the instruction provided. *)

val step_none : int -> Cpu.t -> Cpu.t
(** [step_none mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the none memory mode and the matched instruction. *)

val step_accm : int -> Cpu.t -> Cpu.t
(** [step_accm mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the accumulator memory mode and the matched instruction. *)

val step_abst : int -> Cpu.t -> Cpu.t
(** [step_abst mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the absolute memory mode and the matched instruction. *)

val step_absx : int -> Cpu.t -> Cpu.t
(** [step_absx mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the absolute x memory mode and the matched instruction. *)

val step_absy : int -> Cpu.t -> Cpu.t
(** [step_absy mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the absolute y memory mode and the matched instruction. *)

val step_imed : int -> Cpu.t -> Cpu.t
(** [step_imed mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the immediate memory mode and the matched instruction. *)

val step_indr : int -> Cpu.t -> Cpu.t
(** [step_indr mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the indirect memory mode and the matched instruction. *)

val step_xind : int -> Cpu.t -> Cpu.t
(** [step_xind mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the x indirect memory mode and the matched instruction. *)

val step_indy : int -> Cpu.t -> Cpu.t
(** [step_indy mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the indirect y memory mode and the matched instruction. *)

val step_relt : int -> Cpu.t -> Cpu.t
(** [step_relt mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the relative memory mode and the matched instruction. *)

val step_zero : int -> Cpu.t -> Cpu.t
(** [step_zero mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the zero memory mode and the matched instruction. *)

val step_zerx : int -> Cpu.t -> Cpu.t
(** [step_zerx mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the zero x memory mode and the matched instruction. *)

val step_zery : int -> Cpu.t -> Cpu.t
(** [step_zery mode cpu] matches the mode to an instruction and performs one step
    of updating the Cpu using the zero y memory mode and the matched instruction. *)

val step : Cpu.t -> uint8 -> Cpu.t
(** [step cpu mode] matches the mode to a memory mode and then performs one step
    of updating the Cpu using the matched memory mode. *)
