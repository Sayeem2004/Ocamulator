open UInt8

module UInt16Set : UIntSet.S with type t := int
(** [UInt16Set] describes an unsigned integer with 16 bits by specifying min and
    max values along with the number of bits. *)

(** [UInt16] represents an unsigned integer with 16 bits that has defined integer
    operations. *)
module UInt16 : sig
    include UInt.S

    val ui16_from_ui8 : uint8 -> t
    val ui16_combine_ui8 : uint8 -> uint8 -> t
end

type uint16 = UInt16.t
(** Alias for UInt16.t. *)

val ( <??> ) : uint16 -> uint16 -> int
(** Alias for UInt16.compare. *)

val ( <--> ) : uint16 -> uint16 -> bool
(** Alias for UInt16.equal. *)

val ( +++ ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.add. *)

val ( !++ ) : uint16 -> uint16
(** Alias for UInt16.succ. *)

val ( --- ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.sub. *)

val ( !-- ) : uint16 -> uint16
(** Alias for UInt16.pred. *)

val ( *** ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.mul. *)

val ( /// ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.div. *)

val ( %%% ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.rem. *)

val ( &&& ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.logand. *)

val ( ||| ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.logor. *)

val ( <<< ) : uint16 -> int -> uint16
(** Alias for UInt16.shift_left. *)

val ( >>> ) : uint16 -> int -> uint16
(** Alias for UInt16.shift_right. *)

val ( ~^ ) : int -> uint16
(** Alias for UInt16.from_int. *)

val ( ?^ ) : bool -> uint16
(** Converts a bool to a uint8. [true] -> [1], [false] -> [0]. *)

val ( !^ ) : uint8 -> uint16
(** Alias for UInt16.ui16_from_ui8. *)

val ( !. ) : uint16 -> uint8
(** Converts the 8 least significant bits of a uint16 into a uint8. *)
