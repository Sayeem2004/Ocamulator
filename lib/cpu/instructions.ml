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

  let decode_operand (cpu : CPU.t) (mode : memory_mode) : uint16 =
    match mode with
    | Accumulator -> UInt16.ui16_from_ui8 cpu.accumulator
    | Absolute -> CPU.fetch_ui16 cpu cpu.program_counter
    | AbsoluteX -> CPU.fetch_ui16 cpu cpu.program_counter 
    |> UInt16.add (UInt16.ui16_from_ui8 cpu.register_X)
    | AbsoluteY -> CPU.fetch_ui16 cpu cpu.program_counter 
    |> UInt16.add (UInt16.ui16_from_ui8 cpu.register_Y)
    | Immediate -> CPU.fetch_ui8 cpu cpu.program_counter |> UInt16.ui16_from_ui8
    | Implied -> UInt16.zero
    | Indirect -> CPU.fetch_ui16 cpu cpu.program_counter 
    |> CPU.fetch_ui16 cpu
    | XIndirect -> CPU.fetch_ui8 cpu cpu.program_counter
    |> UInt8.add cpu.register_X |> UInt16.ui16_from_ui8
    | IndirectY -> CPU.fetch_ui8 cpu cpu.program_counter 
    |> UInt8.add cpu.register_Y |> UInt16.ui16_from_ui8
    | Relative -> CPU.fetch_ui8 cpu cpu.program_counter |> UInt16.ui16_from_ui8
    |> UInt16.add cpu.program_counter
    | Zeropage -> CPU.fetch_ui8 cpu cpu.program_counter |> UInt16.ui16_from_ui8 
    | ZeropageX -> CPU.fetch_ui8 cpu cpu.program_counter 
    |> UInt8.add cpu.register_X |> UInt16.ui16_from_ui8
    | ZeropageY -> CPU.fetch_ui8 cpu cpu.program_counter 
    |> UInt8.add cpu.register_Y |> UInt16.ui16_from_ui8

    let adc_op (cpu : CPU.t) (mode : memory_mode) =
      let add_op = decode_operand cpu mode in
      {cpu with accumulator = UInt8.add cpu.accumulator (add_op |> UInt16.to_int |> UInt8.from_int) }

    let and_op (cpu : CPU.t) (mode : memory_mode) = 
      let add_op = decode_operand cpu mode in
      {cpu with accumulator = UInt8.logand cpu.accumulator (add_op |> UInt16.to_int |> UInt8.from_int) }
    
    let asl_op (cpu : CPU.t) (mode : memory_mode) =
      if (mode = Accumulator) then {cpu with accumulator = UInt8.shift_left cpu.accumulator 1 }
      else Ram.RAM.write_ui8
end
