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
        accumulator : UIntVar.UIntVar.uint_var;
        register_X : UIntVar.UIntVar.uint_var;
        register_Y : UIntVar.UIntVar.uint_var;
        program_counter : UIntVar.UIntVar.uint_var;
        ram : Ram.RAM.t;
        flags : cpu_flags;
    };;

    val fetch : t -> UIntVar.UIntVar.uint_var -> UIntVar.UIntVar.uint_var;;
end
