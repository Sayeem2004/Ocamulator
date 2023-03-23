(** [UIntSet.S] is a signature for the bounds of an unsigned int type. The
    representative type [t] is most likely to be an integer, but it doesn't have
    to be. The values below perfectly define an unsigned integer of a certain
    size. *)
module type S = sig
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
