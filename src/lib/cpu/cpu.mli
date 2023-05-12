open Alias

(** [Cpu.ml] represents the computer processing unit of the emulator. *)

type flags = {
    negative : bool;
    overflow : bool;
    reserved : bool;
    break : bool;
    decimal : bool;
    interrupt : bool;
    zero : bool;
    carry : bool;
}
(** [flags] is a record type that represents all [8] possible flags as
    [8] [bool]s. *)

type t = {
    accumulator : uint8;
    registerX : uint8;
    registerY : uint8;
    progCounter : uint16;
    stackPointer : uint8;
    flags : flags;
    ram : Ram.t;
}
(** [t] is a record type containing all necessary data types that a CPU
    could possibly interact with.  *)

val flags_to_ui8 : flags -> uint8
(** [flags_to_ui8 cpu] is the [uint8] representation of the [cpu] flags. *)

val flags_from_ui8 : uint8 -> flags
(** [flags_from_ui8 flags] is flags read in from the uint8 argument. *)

val nes_cpu : uint16 -> Ram.t -> t
(** [nes_cpu pc ram] is a [CPU.t] with the given program_counter and ram. *)

val spec_cpu : uint16 -> uint8 -> uint8 -> uint8 -> uint8 -> uint8 -> Ram.t -> t
(** [spec_cpu pc acc x y sp ram flags] is a [t] with the given program_counter,
    accumulator, register_X, register_Y, stack_pointer, ram, and flags.  *)

val fetch_ui8 : t -> uint16 -> uint8
(** [fetch_ui8 cpu addr] is the [uint8] found in the index [addr] in the RAM
    array. *)

val fetch_ui16 : t -> uint16 -> uint16
(** [fetch_ui16 cpu addr] is the [uint16] found in the index [addr] in the
    RAM array. *)

val fetch_instruction : t -> uint8
(** [fetch_instruction cpu] is the [uint8] found at the index of the
    [program_counter] of the given [cpu] *)

val write_ui8 : t -> uint16 -> uint8 -> unit
(** [write_ui8 cpu addr value] writes [value] to the index [addr] in the
    RAM array. *)

val absolute_stack : t -> uint16
(** [absolute_stack cpu] is the absolute location of the stack pointer
    in the RAM array. *)

val push_stack_ui8 : t -> uint8 -> t
(** [push_stack_u8 cpu value] pushes [value] to the stack and updates the
    position of the stack_pointer. *)

val push_stack_ui16 : t -> uint16 -> t
(** [push_stack_u16 cpu value] pushes [value] to the stack and updates the
    position of the stack_pointer. *)

val pop_stack_ui8 : t -> t
(** [pop_stack cpu] pops the top [uint8]
    value of the stack and updates the position of the stack_pointer. *)

val pop_stack_ui16 : t -> t
(** [pop_stack cpu] pops the top [uint16]
    value of the stack and updates the position of the stack_pointer. *)

val peek_stack_ui8 : t -> uint8
(** [peek_stack cpu] is the [uint8] value at the top of the stack. *)

val peek_stack_ui16 : t -> uint16
(** [peek_stack cpu] is the [uint8] value at the top of the stack. *)

val to_string : t -> string
(** [to_string cpu] is a string representation of the cpu. *)
