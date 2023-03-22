module type S = sig
    type t

    val max_value : t
    val zero : t
    val one : t
    val size : int
end
