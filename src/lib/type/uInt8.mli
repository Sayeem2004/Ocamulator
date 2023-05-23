(** [UInt8.ml] simulates an unsigned integer with a size of 1 byte. *)

module UInt8Set : UIntX.USet with type t := int
(** [UInt8Set] describes an unsigned integer with 8 bits by specifying min and
    max values along with the number of bits. *)

module UInt8 : UIntX.UInt
(** [UInt8] represents an unsigned integer with 8 bits that has defined integer
    operations. *)

include UIntX.UInt
(** Including the signature of an arbitrary unsigned integer. *)

val ui4_from_ui8 : t -> UInt4.t
(** [ui4_from_ui8 ui8] is [ui8] >> 4 as a [UInt4] *)

val ui8_from_ui4 : UInt4.t -> t
(** [ui8_from_ui4 ui4] is [ui4] converted to a [UInt8] *)

val combine_ui4 : UInt4.t -> UInt4.t -> t
(** [combine_ui4 hi low] is hi << 4 + low *)
