open UInt8
open UInt16
open Ram

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
        stack_pointer : UInt8.uint8;
        ram : RAM.t;
        flags : cpu_flags;
    }
    (** [CPU.t] is a record type containing all necessary data types that a CPU
        could possibly interact with.  *)

    val flags_ui8 : t -> UInt8.uint8;; 

    val fetch_ui16 : t -> uint16 -> uint16
    (** [fetch_ui16 cpu addr] is the [uint16] found in the index [addr] in the
        RAM array. *)
    val write_ui8 : t -> UInt16.uint16 -> UInt8.uint8 -> unit;;

    val push_stack_u8 : t -> UInt8.uint8 -> t
    val push_stack_u16 : t -> UInt16.uint16 -> t
    val peek_stack : t -> UInt8.uint8
    val pop_stack : t -> t
end
