(** [UInt16.ml] simulates an unsigned integer with a size of 2 bytes. *)

module UInt16Set : UIntX.USet with type t := int
(** [UInt16Set] describes an unsigned integer with 16 bits by specifying min and
    max values along with the number of bits. *)

module UInt16 : UIntX.UInt
(** [UInt16] represents an unsigned integer with 16 bits that has defined integer
    operations. *)

include UIntX.UInt
(** Including the signature of an arbitrary unsigned integer. *)

val ui16_from_ui8 : UInt8.t -> t
(** [ui16_from_ui8 ui8] is [ui8] stored as the representative type of [UInt16]. *)

val combine_ui8 : UInt8.t -> UInt8.t -> t
(** [combine_ui8 hig low] is [hig << 8 + low]. *)

val split_ui16 : t -> UInt8.t * UInt8.t
(** [split_ui16 ui16] is [hig * low] where [ui16 = hig << 8 + low]. *)
