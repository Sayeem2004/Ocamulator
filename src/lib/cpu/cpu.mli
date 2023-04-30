open Ram
open UInt8
open UInt16

(** [Cpu.CPU] represents the CPU of the emulator by storing RAM, all necessary
    flags, and special [uint]s used as special storage options. It also provides
    aliases for accessing the RAM array. *)
module CPU : sig
    type cpu_flags = {
        carr_bit : bool;
        zero : bool;
        interrupt : bool;
        decimal : bool;
        negative : bool;
        overflow : bool;
        break : bool;
        reserved : bool;
    }
    (** [CPU.cpu_flags] is a record type that represents all [8] possible flags as
        [8] [bool]s. *)

    type t = {
        accumulator : uint8;
        register_X : uint8;
        register_Y : uint8;
        program_counter : uint16;
        stack_pointer : uint8;
        ram : RAM.t;
        flags : cpu_flags;
    }
    (** [CPU.t] is a record type containing all necessary data types that a CPU
        could possibly interact with.  *)

    val def_flag : unit -> cpu_flags
    (** [def_flag ()] is the default [cpu_flags] with all flags set to false except
        the reserved flag which is true. *)

    val nes_cpu : uint16 -> RAM.t -> t
    (** [nes_cpu pc ram] is a [CPU.t] with the given program_counter and ram. *)

    val flags_ui8 : t -> uint8
    (** [flags_ui8 cpu] is the [uint8] representation of the [cpu] flags. *)

    val flags_from_ui8 : t -> uint8 -> t
    (** [flags_from_ui8 cpu flags] is cpu with flags read in from the uint8 arg. *)

    val spec_cpu : uint16 -> uint8 -> uint8 -> uint8 -> uint8 -> uint8 -> t
    (** [spec_cpu pc acc x y sp flags] is a [CPU.t] with the given program_counter,
        accumulator, register_X, register_Y, stack_pointer, and flags. The ram is
        set to the zero array. *)

    val fetch_ui8 : t -> uint16 -> uint8
    (** [fetch_ui8 cpu addr] is the [uint8] found in the index [addr] in the RAM
        array. *)

    val fetch_ui16 : t -> uint16 -> uint16
    (** [fetch_ui16 cpu addr] is the [uint16] found in the index [addr] in the
        RAM array. *)

    val fetch_current_instruction : t -> uint8
    (** [fetch_current_instruction] is the [uint8] found at the index of the
        [program_counter] of the given [t] *)

    val write_ui8 : t -> uint16 -> uint8 -> unit
    (** [write_ui8 cpu addr value] writes [value] to the index [addr] in the
        RAM array. *)

    val absolute_loc_stack : t -> uint16
    (** [absolute_loc_stack cpu] is the absolute location of the stack pointer
        in the RAM array. *)

    val push_stack_ui8 : t -> uint8 -> t
    (** [push_stack_u8 cpu value] pushes [value] to the stack and updates the
        position of the stack_pointer. *)

    val push_stack_ui16 : t -> uint16 -> t
    (** [push_stack_u16 cpu value] pushes [value] to the stack and updates the
        position of the stack_pointer. *)

    val peek_stack_ui8 : t -> uint8
    (** [peek_stack cpu] is the [uint8] value at the top of the stack. *)

    val peek_stack_ui16 : t -> uint16
    (** [peek_stack cpu] is the [uint8] value at the top of the stack. *)

    val pop_stack_ui8 : t -> t
    (** [pop_stack cpu] pops the top [uint8]
        value of the stack and updates the position of the stack_pointer. *)

    val pop_stack_ui16 : t -> t
    (** [pop_stack cpu] pops the top [uint16]
        value of the stack and updates the position of the stack_pointer. *)
end
