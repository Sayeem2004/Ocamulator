open Alias

(** [Ram.ml] simulates random access memory as an array of fixed length. *)

type t = {
    size : int;
    memory : bytes;
}
(** [t] is a record type that stores the array representing RAM and an
    int representing its size. *)

val zero_ram : unit -> t
(** [zero_ram ()] is an instantiation of type [t] with size equivalent
    to the [UInt16.max + 1]. *)

val nes_ram : bytes -> t
(** [nes_ram b] is an instantiation of type [t] containing the bytes
    sequence given by [b]. *)

val read_ui8 : t -> uint16 -> uint8
(** [read_ui8 ram addr] is the [uint8] stored in the index of [ram] at
    [addr]. *)

val write_ui8 : t -> uint16 -> uint8 -> unit
(** [write_ui8 ram addr val] writes [val] to the index of [ram] at
    [addr]. *)
