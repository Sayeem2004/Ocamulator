(** [UIntX.F] is a functor that takes in a [UIntSet.S] describing the bounds of
    and unsigned integer and transforms it into a [UInt.S] that contains both
    the bounds and common operations on the unsigned integer. *)
module F (M : UIntSet.S with type t := int) : UInt.S
