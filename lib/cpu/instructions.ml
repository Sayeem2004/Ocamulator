open UInt8
open UInt16
open Cpu

module Instructions = struct
  type _ memory_mode =
  | Accumulator : uint8 memory_mode
  | Absolute : uint16 -> uint16 memory_mode
  | AbsoluteX : uint16 -> uint16 memory_mode
  | AbsoluteY : uint16 -> uint16 memory_mode
  | Immediate : uint8 -> uint8 memory_mode
  | Implied : uint8 option -> uint8 option memory_mode
  | Indirect : uint16 -> uint16 memory_mode
  | XIndirect : uint8 -> uint16 memory_mode
  | IndirectY : uint8 -> uint16 memory_mode
  | Relative : uint8 -> uint16 memory_mode
  | Zeropage : uint8 -> uint8 memory_mode
  | ZeropageX : uint8 -> uint16 memory_mode
  | ZeropageY : uint8 -> uint16 memory_mode

  let decode_operand (cpu : CPU.t) (type a) (mode : a memory_mode) : a =
    match mode with
    | Accumulator -> cpu.accumulator
    | Absolute addr -> addr
    | AbsoluteX addr -> addr +++ !^ (cpu.register_X) +++ ?^ (cpu.flags.carr_bit)
    | AbsoluteY addr -> addr +++ !^ (cpu.register_Y) +++ ?^ (cpu.flags.carr_bit)
    | Immediate b -> b
    | Implied u_opt -> u_opt
    | Indirect addr -> CPU.fetch_ui16 cpu addr
    | XIndirect addr -> CPU.fetch_ui16 cpu !^ (addr ++ cpu.register_X) 
    | IndirectY u8_addr -> CPU.fetch_ui16 cpu !^ (u8_addr) +++ !^ 
    (cpu.register_Y) +++ ?^ (cpu.flags.carr_bit)
    | Relative b -> cpu.program_counter +++ !^ b
    | Zeropage z_addr -> z_addr  
    | ZeropageX u8_addr -> !^ u8_addr +++ !^ (cpu.register_X)
    | ZeropageY u8_addr -> !^ u8_addr +++ !^ (cpu.register_Y)

  let adc_op (cpu : CPU.t) (mode : uint8 memory_mode) : CPU.t =
    match decode_operand cpu mode with
    | u_8 -> {cpu with accumulator = u_8 ++ cpu.accumulator }
end