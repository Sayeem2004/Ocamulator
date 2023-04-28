(** [UIntX.USet] is a signature for the bounds of an unsigned int type. The
    representative type [t] is most likely to be an integer, but it doesn't have
    to be. The values below perfectly define an unsigned integer of a certain
    size. *)
module type USet = sig
    type t
    (** Representative type of this signature, most likely an integer. *)

    val max_value : t
    (** [max_value ()] is the maximum value of this unsigned int type. If [n] is
        an unsigned int then it is always true that [n <= max_value ()]*)

    val zero : t
    (** [zero ()] is the minimum value of this unsigned int type, which is
        always [0]. If [n] is an unsigned int then it is always true that
        [n >- zero ()] *)

    val one : t
    (** [one ()] is exactly the value [1], which should exist for any unsigned
        int type. *)

    val size : int
    (** [size ()] is the number of bits in this unsigned int type.
        [size () >= 1] for any unsigned int type. *)
end

(** [UIntX.UInt] is a signature for a generic unsigned int type. The representative
    type [t] is most likely to be an integer, but it doesn't have to be. The
    functions below all represent common operations on integers, but they have
    to be modified to be within the range specified by the unsigned int type. *)
module type UInt = sig
    type t
    (** Representative type of this signature, most likely an integer. *)

    val to_string : t -> string
    (** [to_string a] is the string representation of the unsigned integer [a]. *)

    val max_value : t
    (** [max_value ()] is the maximum value of this unsigned int type. If [n] is
        an unsigned int then it is always true that [n <= max_value ()]*)

    val zero : t
    (** [zero ()] is the minimum value of this unsigned int type, which is
        always [0]. If [n] is an unsigned int then it is always true that
        [n >- zero ()] *)

    val one : t
    (** [one ()] is exactly the value [1], which should exist for any unsigned
        int type. *)

    val size : int
    (** [size ()] is the number of bits in this unsigned int type.
        [size () >= 1] for any unsigned int type. *)

    val compare : t -> t -> int
    (** [compare a b] is [< 0] if [a < b], [0] is [a == b], and [> 0] if
        [a > b]. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] if [a == b] and [false] otherwise. Equivalent to
        [compare a b] when [a == b]. *)

    val add : t -> t -> t
    (** [add a b] is [a + b mod 2^size]. *)

    val sub : t -> t -> t
    (** [sub a b] is [a - b mod 2^size]. This is not well-defined when [b > a]. *)

    val mul : t -> t -> t
    (** [mul a b] is [a * b mod 2^size]. *)

    val div : t -> t -> t
    (** [div a b] is [a / b mod 2^size]. This is not well-defined when [b == 0]. *)

    val rem : t -> t -> t
    (** [rem a b] is [a % b mod 2^size]. This is not well-defined when [b == 0]. *)

    val succ : t -> t
    (** [succ a] is [a + 1 mod 2^size]. *)

    val pred : t -> t
    (** [pred a] is [a - 1 mod 2^size]. *)

    val logand : t -> t -> t
    (** [logand a b] is [a & b mod 2^size]. *)

    val logor : t -> t -> t
    (** [logor a b] is [a | b mod 2^size]. *)

    val logxor : t -> t -> t
    (** [logxor a b] is [a ^ b mod 2^size]. *)

    val shift_left : t -> int -> t
    (** [shift_left a n] is [a << n mod 2^size]. *)

    val shift_right : t -> int -> t
    (** [shift_right a n] is [a >> n mod 2^size]. *)

    val from_int : int -> t
    (** [from_int a] is [a mod 2^size]. *)

    val to_int : t -> int
    (** [to_int a] is [a]. *)
end

(** [UIntX.SetToInt] is a functor that takes in a [UIntX.USet] describing the
    bounds of and unsigned integer and transforms it into a [UIntX.UInt] that
    contains both the bounds and common operations on the unsigned integer. *)
module SetToInt (M : USet with type t := int) : UInt
