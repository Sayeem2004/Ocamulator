open Alias

(** [Decode.ml] provides utilities for Cpu and opcode inter-communication. *)

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

val contents : Cpu.t -> 'a memory_mode -> uint8
(** [contents cpu mode] is the contents stored at the address the memory
    mode represents. *)

val address : Cpu.t -> 'a memory_mode -> uint16
(** [address cpu mode] is the address the memory mode represents. *)

val fetch_ui8_op : Cpu.t -> uint8
(** [fetch_ui8_op cpu] is the next uint8 in RAM specified by the program
    counter. *)

val increment_pc : Cpu.t -> int -> Cpu.t
(** [increment_pc cpu n] is [cpu] with the program counter incremented by
    [n]. *)

val fetch_ui16_op : Cpu.t -> uint16
(** [fetch_ui16_op cpu] is the next uint16 in RAM specified by the program
    counter. *)
