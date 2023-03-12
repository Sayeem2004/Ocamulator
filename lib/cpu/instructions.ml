open Lib__UInt8
open Lib__UInt16

open Lib__Cpu

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

  let decode_operand (cpu : CPU.t) (mode : memory_mode) =
    match mode with
    | Accumulator -> cpu.accumulator
    | _ -> cpu.accumulator (** TODO *)
end
