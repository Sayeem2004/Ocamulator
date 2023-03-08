open UInt8;;
(* open UInt16 *)
open Cpu;;

module Instructions = struct
    type memory_mode =
        | Accumulator
        | Absolute
        | AbsoluteX
        | AbsoluteY
        | Immediate
        | Implied
        | Indirect
        | XIndirect
        | IndirectY
        | Relative
        | Zeropage
        | ZeropageX
        | ZeropageY
    ;;

    let decode_operand (cpu: CPU.t) (mode: memory_mode) : uint8 =
        match mode with
        | Accumulator -> cpu.accumulator
        | _ -> cpu.accumulator (** TODO *)
    ;;
end
