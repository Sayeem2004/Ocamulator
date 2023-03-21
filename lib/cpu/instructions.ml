open UIntVar.UIntVar
open Cpu

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

  let decode_operand (cpu : CPU.t) (mode : memory_mode) : uint_var =
    match mode with
    | Accumulator -> cpu.accumulator
    | Absolute -> CPU.fetch cpu cpu.program_counter
    | AbsoluteX -> CPU.fetch cpu cpu.program_counter ++ cpu.register_X
    | AbsoluteY -> CPU.fetch cpu cpu.program_counter ++ cpu.register_Y
    | Immediate -> CPU.fetch cpu cpu.program_counter
    | Implied -> Temp16(0)
    | Indirect -> CPU.fetch cpu cpu.program_counter |> CPU.fetch cpu
    | XIndirect -> CPU.fetch cpu cpu.program_counter ++ cpu.register_X
    | IndirectY -> CPU.fetch cpu cpu.program_counter ++ cpu.register_Y
    | Relative -> CPU.fetch cpu cpu.program_counter ++ cpu.program_counter
    | Zeropage -> CPU.fetch cpu cpu.program_counter 
    | ZeropageX -> CPU.fetch cpu cpu.program_counter ++ cpu.register_X
    | ZeropageY -> CPU.fetch cpu cpu.program_counter ++ cpu.register_Y

    let adc_op (cpu : CPU.t) (mode : memory_mode) =
      let add_op = decode_operand cpu mode in
      {cpu with accumulator = add_op ++ cpu.accumulator }

    let and_op (cpu : CPU.t) (mode : memory_mode) = 
      let add_op = decode_operand cpu mode in
      {cpu with accumulator = add_op && cpu.accumulator }

end