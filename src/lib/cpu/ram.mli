open UInt8
open UInt16

(** [Ram.RAM] simulates random access memory as an array of fixed length,
    specifically of size [2^16 + 1]. It also provides utilities for reading and
    writing to this array. *)
module RAM : sig
    type t = {
        max_mem : int;
        ram_memory : bytes;
    }
    (** [RAM.t] is a record type that stores the array representing RAM and an
        int representing its size. *)

    val nes_zero_ram : unit -> t
    (** [nes_zero_ram ()] is an instantiation of type [t] with size equivalent
        to the [UInt16.max_value + 1]. *)

    val nes_ram : bytes -> t
    (** [nes_ram b] is an instantiation of type [t] containing the bytes
        sequence given by [b] *)

    val read_ui8 : t -> uint16 -> uint8
    (** [read_ui8 ram addr] is the [uint8] stored in the index of [ram] at
        [addr]. *)

    val read_ui16 : t -> uint16 -> uint16
    (** [read_ui16 ram addr] is the [uint16] formed from combining the two
        [uint8]'s stored in [ram] at [addr] and [addr + 1]. *)

    val write_ui8 : t -> uint16 -> uint8 -> unit
    (** [write_ui8 ram addr val] writes [val] to the index of [ram] at
        [addr]. *)

    val write_ui16 : t -> uint16 -> uint16 -> unit
    (** [write_ui16 ram addr val] writes [val] to the indices of [ram] at
        [addr] and [addr + 1]. *)
end
