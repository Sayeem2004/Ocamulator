(** [UInt4.ml] simulates an unsigned integer with a size of 4 bits. *)

module UInt4Set : UIntX.USet with type t := int
(** [UInt4Set] describes an unsigned integer with 4 bits by specifying min and
    max values along with the number of bits. *)

module UInt4 : UIntX.UInt
(** [UInt4] represents an unsigned integer with 4 bits that has defined integer
    operations. *)

include UIntX.UInt
(** Including the signature of an arbitrary unsigned integer. *)
