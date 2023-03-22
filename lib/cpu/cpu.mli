module CPU : sig
    type cpu_flags = {
        carr_bit : bool;
        zero : bool;
        interrupt : bool;
        decimal : bool;
        negative : bool;
        overflow : bool;
        break : bool;
        reserved : bool;
    };;

    type t = {
        accumulator : UInt8.uint8;
        register_X : UInt8.uint8;
        register_Y : UInt8.uint8;
        program_counter : UInt16.uint16;
        stack_pointer : UInt8.uint8;
        ram : Ram.RAM.t;
        flags : cpu_flags;
    };;

    val flags_ui8 : t -> UInt8.uint8;; 

    val fetch_ui8 : t -> UInt16.uint16 -> UInt8.uint8;;
    val fetch_ui16 : t -> UInt16.uint16 -> UInt16.uint16;;
    val write_ui8 : t -> UInt16.uint16 -> UInt8.uint8 -> unit;;

    val push_stack_u8 : t -> UInt8.uint8 -> t
    val push_stack_u16 : t -> UInt16.uint16 -> t
    val peek_stack : t -> UInt8.uint8
    val pop_stack : t -> t
end
