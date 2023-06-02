(** [Alias.ml] contains helpful infix operators to reduce code repetition. *)
type interrupt =
    | NMI
    | IRQ
    | RESET

type joypad_button =
    | Right
    | Left
    | Down
    | Up
    | Start
    | Select
    | Button_B
    | Button_A

val interrupt_to_string : interrupt -> string

type master =
    | E_B
    | E_C

type mirroring =
    | HORIZONTAL
    | VERTICAL
    | SINGLE
    | QUAD

type uint4 = UInt4.t
(** Alias for UInt4.t. *)

type uint8 = UInt8.t
(** Alias for UInt8.t. *)

type uint16 = UInt16.t
(** Alias for UInt16.t. *)

exception ReadError of uint16
exception WriteError of uint16
exception InvalidValue

val ( ~... ) : int -> uint4
(** Infix alias for UInt4.from_int. *)

val ( <?> ) : uint8 -> uint8 -> int
(** Infix alias for UInt8.compare. *)

val ( <-> ) : uint8 -> uint8 -> bool
(** Infix alias for UInt8.equal. *)

val ( ++ ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.add. *)

val ( -- ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.sub. *)

val ( ** ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.mul *)

val ( // ) : uint8 -> uint8 -> uint8
(** Infix alias for UInt8.div *)

val ( !. ) : uint8 -> uint8
(** Infix alias for UInt8.lognot *)

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

val ( ?& ) : uint8 -> int -> bool
(** Returns true if the nth bit (0-based) of [ui8] is 1, false otherwise *)

val ( !& ) : uint8 -> int -> bool -> uint8
(** Returns [ui8] with the nth bit (0-based) set to 1 if [true] and [ui8] with the nth bit set to 0 if [false] *)

val ( <??> ) : uint16 -> uint16 -> int
(** Alias for UInt16.compare. *)

val ( /> ) : uint8 -> uint8 -> bool
(** Equivalent of greater than for uint8 *)

val ( />= ) : uint8 -> uint8 -> bool
(** Equivalent of greater than or equal to for uint8 *)

val ( /< ) : uint8 -> uint8 -> bool
(** Equivalent of lesser than for uint8 *)

val ( /<= ) : uint8 -> uint8 -> bool
(** Equivalent of lesser than or equal to for uint8 *)

val ( <--> ) : uint16 -> uint16 -> bool
(** Alias for UInt16.equal. *)

val ( +++ ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.add. *)

val ( --- ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.sub. *)

val ( *** ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.mul *)

val ( /// ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.div *)

val ( &&& ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.logand. *)

val ( ||| ) : uint16 -> uint16 -> uint16
(** Alias for UInt16.logor *)

val ( <<< ) : uint16 -> int -> uint16
(** Alias for UInt16.shift_left. *)

val ( >>> ) : uint16 -> int -> uint16
(** Alias for UInt16.shift_right. *)

val ( ~.. ) : int -> uint16
(** Alias for UInt16.from_int. *)

val ( ~** ) : uint16 -> int
(** Alias for UInt16.to_int. *)

val ( !... ) : uint16 -> uint16
(** Alias for UInt16.lognot *)

val ( !.. ) : uint8 -> uint8 -> uint16
(** Alias for UInt16.combine_ui8. *)

val ( !** ) : uint8 -> uint16
(** Alias for UInt16.ui16_from_ui8. *)

val ( !*** ) : uint16 -> uint8
(** Alias for [UInt16.to_int |> UInt8.from_int] *)

val ( !-- ) : uint16 -> uint8
(** Converts the 8 least significant bits of a uint16 into a uint8. *)

val ( !++ ) : bool -> uint16
(** Converts a bool to a uint16. [true] -> [1], [false] -> [0]. *)

val ( !@@ ) : uint16 -> uint8 * uint8
(** Converts a uint16 into two uint8s. *)

val ( //> ) : uint16 -> uint16 -> bool
(** Equivalent of greater than for uint16 *)

val ( //>= ) : uint16 -> uint16 -> bool
(** Equivalent of greater than or equal to for uint16 *)

val ( //< ) : uint16 -> uint16 -> bool
(** Equivalent of less than for uint16 *)

val ( //<= ) : uint16 -> uint16 -> bool
(** Equivalent of less than or equal to for uint16 *)
