open Cpu
open UInt8
open UInt16

(** The Nintendo Entertainment System had 255 opcodes that the CPU could run.
    Each would modify the CPU in their own way, but their functionality could
    be generalized into 56 instructions. The [Decode.Decode] module provides
    utilities to help with this grouping and to provide data to the CPU
    instructions. *)
module Decode : sig
    type _ memory_mode =
        | Accumulator : uint8 memory_mode
        | Absolute : uint16 -> uint16 memory_mode
        | AbsoluteX : uint16 -> uint16 memory_mode
        | AbsoluteY : uint16 -> uint16 memory_mode
        | Immediate : uint8 -> uint8 memory_mode
        | Indirect : uint16 -> uint16 memory_mode
        | XIndirect : uint8 -> uint16 memory_mode
        | IndirectY : uint8 -> uint16 memory_mode
        | Relative : uint8 -> uint16 memory_mode
        | Zeropage : uint8 -> uint8 memory_mode
        | ZeropageX : uint8 -> uint16 memory_mode
        | ZeropageY : uint8 -> uint16 memory_mode
        (** [memory_mode] are the possible memory types that can be provided to an
            instruction. *)

    val contents : CPU.t -> 'a memory_mode -> uint8
    (** [contents cpu mode] is the contents stored at the address the memory
        mode represents. *)

    val address : CPU.t -> 'a memory_mode -> uint16
    (** [address cpu mode] is the address the memory mode represents. *)

    val add_overflow : uint8 -> uint8 -> uint8 -> bool
    (** [add_overflow a b c] is true if the addition of [a] and [b] overflows to
        [c]. *)

    val sub_overflow : uint8 -> uint8 -> uint8 -> bool
    (** [sub_overflow a b c] is true if the subtraction of [a] and [b] overflow
        to [c]. *)

    val incr_cpu_pc : CPU.t -> int -> CPU.t
    (** [incr_cpu_pc cpu n] is [cpu] with the program counter incremented by
        [n]. *)

    val fetch_uint8_op : CPU.t -> uint8
    (** [fetch_uint8_op cpu] is the next uint8 in RAM specified by the program
        counter. *)

    val fetch_uint16_op : CPU.t -> uint16
    (** [fetch_uint16_op cpu] is the next uint16 in RAM specified by the program
        counter. *)
end
