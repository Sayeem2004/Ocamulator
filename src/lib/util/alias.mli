(** [Alias.ml] contains helpful infix operators to reduce code repetition. *)

type uint8 = UInt8.t
(** Alias for UInt8.t. *)

type uint16 = UInt16.t
(** Alias for UInt16.t. *)

val ( <?> ) : uint8 -> uint8 -> int
(** Infix alias for UInt8.compare. *)

val ( <-> ) : uint8 -> uint8 -> bool
(** Infix alias for UInt8.equal. *)

val ( ++ ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.add. *)

val ( -- ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.sub. *)

val ( &&. ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.logand. *)

val ( |&. ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.logxor. *)

val ( ||. ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.logor. *)

val ( << ) : uint8 -> int -> uint8
(** Infix alias for UInt8.shift_left. *)

val ( >> ) : uint8 -> int -> uint8
(** Infix alias for UInt8.shift_right. *)

val ( ~. ) : int -> uint8
(** Infix alias for UInt8.from_int. *)

val ( ~* ) : uint8 -> int
(** Infix alias for UInt8.to_int. *)

val ( ?. ) : bool -> uint8
(** Converts a bool to a uint8. [true] -> [1], [false] -> [0]. *)

val ( ?* ) : uint8 -> bool
(** Returns true if the [uint8] is [0] else false. *)

val ( ?- ) : uint8 -> bool
(** Returns true if the most significant bit of a uint8 is [1]. *)

val ( ?+ ) : uint8 -> bool
(** Returns true if the least significant bit of a uint8 is [1]. *)

val ( ?@ ) : uint8 -> uint8
(** Returns the two's complement of a uint8. *)

val ( ?% ) : uint8 -> int
(** Returns the signed integer of a uint8. *)

val ( <??> ) : uint16 -> uint16 -> int
(** Alias for UInt16.compare. *)

val ( <--> ) : uint16 -> uint16 -> bool
(** Alias for UInt16.equal. *)

val ( +++ ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.add. *)

val ( --- ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.sub. *)

val ( &&& ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.logand. *)

val ( <<< ) : uint16 -> int -> uint16
(** Alias for UInt16.shift_left. *)

val ( >>> ) : uint16 -> int -> uint16
(** Alias for UInt16.shift_right. *)

val ( ~.. ) : int -> uint16
(** Alias for UInt16.from_int. *)

val ( ~** ) : uint16 -> int
(** Alias for UInt16.to_int. *)

val ( !.. ) : uint8 -> uint8 -> uint16
(** Alias for UInt16.combine_ui8. *)

val ( !** ) : uint8 -> uint16
(** Alias for UInt16.ui16_from_ui8. *)

val ( !-- ) : uint16 -> uint8
(** Converts the 8 least significant bits of a uint16 into a uint8. *)

val ( !++ ) : bool -> uint16
(** Converts a bool to a uint16. [true] -> [1], [false] -> [0]. *)

val ( !@@ ) : uint16 -> uint8 * uint8
(** Converts a uint16 into two uint8s. *)
