module UInt8Set : UIntSet.S with type t := int
(** [UInt8Set] describes an unsigned integer with 8 bits by specifying min and
    max values along with the number of bits. *)

module UInt8 : UInt.S
(** [UInt8] represents an unsigned integer with 8 bits that has defined integer
    operations. *)

type uint8 = UInt8.t
(** Alias for UInt8.t. *)

val ( <?> ) : uint8 -> uint8 -> int
(** Infix alias for UInt8.compare. *)

val ( <-> ) : uint8 -> uint8 -> bool
(** Infix alias for UInt8.equal. *)

val ( ++ ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.add. *)

val ( !+ ) : uint8 -> uint8
(** Infix alias for UInt8.succ. *)

val ( -- ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.sub. *)

val ( !- ) : uint8 -> uint8
(** Infix alias for UInt8.pred. *)

val ( ** ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.mul. *)

val ( // ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.div. *)

val ( %% ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.rem. *)

val ( && ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.logand. *)

val ( || ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.logor. *)

val ( << ) : uint8 -> int -> uint8
(** Infix alias for UInt8.shift_left. *)

val ( >> ) : uint8 -> int -> uint8
(** Infix alias for UInt8.shift_right. *)

val ( ~. ) : int -> uint8
(** Infix alias for UInt8.from_int. *)

val ( ?. ) : bool -> uint8
(** Converts a bool to a uint8. [true] -> [1], [false] -> [0]. *)

val ( ?> ) : uint8 -> uint8 -> bool
(** Returns true if the addition of two uint8's overflow. *)

val ( ?- ) : uint8 -> bool
(** Returns true if the most significant bit of a uint8 is [1]. *)
