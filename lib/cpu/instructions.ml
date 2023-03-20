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

  type operand = {
    u8 : uint8;
    u16 : uint16;
  }

  let decode_operand_ui16 (cpu : CPU.t) (mode : memory_mode) : uint16 =
    match mode with
    | Accumulator -> UInt16.ui16_from_ui8 cpu.accumulator

    | Absolute -> CPU.fetch_ui16 cpu cpu.program_counter

    | AbsoluteX -> UInt16.add (CPU.fetch_ui16 cpu cpu.program_counter) 
    (UInt16.ui16_from_ui8 cpu.register_X) |> UInt16.add (UInt16.ui16_from_ui8 CPU.carr_uint8 cpu.flags)

    | AbsoluteY -> UInt16.add_carry (CPU.fetch_ui16 cpu cpu.program_counter) 
    (UInt16.ui16_from_ui8 cpu.register_Y) cpu.flags.carr_bit

    | Immediate -> CPU.fetch_ui8 cpu cpu.program_counter |> UInt16.ui16_from_ui8

    | Implied -> UInt16.zero (* TODO *)

    | Indirect -> CPU.fetch_ui16 cpu cpu.program_counter 
    |> CPU.fetch_ui16 cpu

    | XIndirect -> CPU.fetch_ui16 cpu cpu.program_counter
    |> UInt16.add (UInt16.ui16_from_ui8 cpu.register_X) |> CPU.fetch_ui8 cpu |> UInt16.ui16_from_ui8

    | IndirectY -> UInt16.add_carry (CPU.fetch_ui16 cpu cpu.program_counter) 
    (UInt16.ui16_from_ui8 cpu.register_Y) cpu.flags.carr_bit |> CPU.fetch_ui8 cpu |> UInt16.ui16_from_ui8

    | Relative -> CPU.fetch_ui8 cpu cpu.program_counter |> UInt16.ui16_from_ui8
    |> UInt16.add cpu.program_counter

    | Zeropage -> CPU.fetch_ui8 cpu cpu.program_counter |> UInt16.ui16_from_ui8

    | ZeropageX -> CPU.fetch_ui8 cpu cpu.program_counter 
    |> UInt8.add cpu.register_X |> UInt16.ui16_from_ui8
    
    | ZeropageY -> CPU.fetch_ui8 cpu cpu.program_counter 
    |> UInt8.add cpu.register_Y |> UInt16.ui16_from_ui8

    let populate_operand_ui16 (ui16 : uint16) =
        {
            u8 = UInt16.to_int ui16 |> UInt8.from_int;
            u16 = ui16;
        }
    
    let decode_operand (cpu : CPU.t) (mode : memory_mode) =
        decode_operand_ui16 cpu mode |> populate_operand_ui16

    let adc_op (cpu : CPU.t) (mode : memory_mode) =
        let operand = decode_operand cpu mode in
        let summed_acc = UInt8.add_carry cpu.accumulator operand.u8 cpu.flags.carr_bit in
        if (UInt8.compare summed_acc cpu.accumulator < 0) then {cpu with accumulator = summed_acc; flags = {cpu.flags with carr_bit = true}} else {cpu with accumulator = summed_acc}

    let and_op (cpu : CPU.t) (mode : memory_mode) =
      let operand = decode_operand cpu mode in
      {cpu with accumulator = UInt8.logand operand.u8 cpu.accumulator}
    
    
end
