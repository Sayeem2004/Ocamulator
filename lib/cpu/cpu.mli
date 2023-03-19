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
        ram : Ram.RAM.t;
        flags : cpu_flags;
    };;

    val fetch_ui8 : t -> UInt16.uint16 -> UInt8.uint8;;
    val fetch_ui16 : t -> UInt16.uint16 -> UInt16.uint16;;
end
