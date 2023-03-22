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

    let decode_contents (cpu : CPU.t) (type a) (mode : a memory_mode) : uint8 =
        match mode with
        | Accumulator -> cpu.accumulator
        | Absolute abs_addr -> CPU.fetch_ui8 cpu abs_addr
        | AbsoluteX abs_addr_x ->
            abs_addr_x +++ !^(cpu.register_X) +++ ?^(cpu.flags.carr_bit)
            |> CPU.fetch_ui8 cpu
        | AbsoluteY abs_addr_y ->
            abs_addr_y +++ !^(cpu.register_Y) +++ ?^(cpu.flags.carr_bit)
            |> CPU.fetch_ui8 cpu
        | Immediate b -> b
        | Indirect ind_addr -> CPU.fetch_ui16 cpu ind_addr |> CPU.fetch_ui8 cpu
        | XIndirect x_ind_addr ->
            !^x_ind_addr +++ !^(cpu.register_X)
            |> CPU.fetch_ui16 cpu |> CPU.fetch_ui8 cpu
        | IndirectY ind_addr_y ->
            CPU.fetch_ui16 cpu !^ind_addr_y
            |> CPU.fetch_ui8 cpu
            |> ( ++ ) (cpu.register_Y ++ ?.(cpu.flags.carr_bit))
        | Relative b -> !^b +++ cpu.program_counter |> CPU.fetch_ui8 cpu
        | Zeropage zero_addr -> CPU.fetch_ui8 cpu !^zero_addr
        | ZeropageX zero_addr_x ->
            !^(zero_addr_x ++ cpu.register_X) |> CPU.fetch_ui8 cpu
        | ZeropageY zero_addr_y ->
            !^(zero_addr_y ++ cpu.register_Y) |> CPU.fetch_ui8 cpu
        | _ -> raise (Failure "AHHHH")

    let decode_address (cpu : CPU.t) (type a) (mode : a memory_mode) : uint16 =
        match mode with
        | Absolute abs_addr -> abs_addr
        | AbsoluteX abs_addr_x ->
            abs_addr_x +++ !^(cpu.register_X) +++ ?^(cpu.flags.carr_bit)
        | AbsoluteY abs_addr_y ->
            abs_addr_y +++ !^(cpu.register_Y) +++ ?^(cpu.flags.carr_bit)
        | Indirect ind_addr -> CPU.fetch_ui16 cpu ind_addr
        | XIndirect x_ind_addr ->
            !^x_ind_addr +++ !^(cpu.register_X) |> CPU.fetch_ui16 cpu
        | IndirectY ind_addr_y -> CPU.fetch_ui16 cpu !^ind_addr_y
        | Relative b -> !^b +++ cpu.program_counter
        | Zeropage zero_addr -> !^zero_addr
        | ZeropageX zero_addr_x -> !^(zero_addr_x ++ cpu.register_X)
        | ZeropageY zero_addr_y -> !^(zero_addr_y ++ cpu.register_Y)
        | _ -> raise (Failure "AHHH")

    let adc_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        let operand = decode_contents cpu mode in
        let summed_acc = cpu.accumulator ++ operand in
        let carry = ?>operand summed_acc in
        let neg_bit = ?-summed_acc in
        {
            cpu with
            accumulator = summed_acc;
            flags = { cpu.flags with carr_bit = carry; negative = neg_bit };
        }
end
