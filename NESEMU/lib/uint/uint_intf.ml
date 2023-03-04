module type Uint =
  sig
    type t

    val to_string : t -> string

    val max_value : t
    val zero : t
    val one : t

    val compare : t -> t -> int
    val equal : t -> t -> bool

    val le : t -> t -> bool
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val rem : t -> t -> t

    val succ : t -> t
    val pred : t -> t
    
    val logand : t -> t -> t
    val logor : t -> t -> t
    val logxor : t -> t -> t
    val shift_left : t -> int -> t
    val shift_right : t -> int -> t
    val from_int : int -> t
  end

module type UintSet = sig
  type t 
  
  val max_value : t
  val zero : t
  val one : t
end