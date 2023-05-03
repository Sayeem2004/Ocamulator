(** [UInt8.ml] simulates an unsigned integer with a size of 1 byte. *)

module UInt8Set : UIntX.USet with type t := int
(** [UInt8Set] describes an unsigned integer with 8 bits by specifying min and
    max values along with the number of bits. *)

module UInt8 : UIntX.UInt
(** [UInt8] represents an unsigned integer with 8 bits that has defined integer
    operations. *)

include UIntX.UInt
(** Including the signature of an arbitrary unsigned integer. *)
