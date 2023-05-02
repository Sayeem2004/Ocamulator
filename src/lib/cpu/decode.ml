open Cpu
open UInt8
open UInt16

module Decode = struct
    type _ memory_mode =
        | Accumulator : uint8 memory_mode
        | Absolute : uint16 -> uint16 memory_mode
        | AbsoluteX : uint16 -> uint16 memory_mode
        | AbsoluteY : uint16 -> uint16 memory_mode
        | Immediate : uint8 -> uint8 memory_mode
        | Indirect : uint16 -> uint16 memory_mode
        | XIndirect : uint8 -> uint16 memory_mode
        | IndirectY : uint8 -> uint16 memory_mode
        | Relative : uint8 -> uint16 memory_mode
        | Zeropage : uint8 -> uint8 memory_mode
        | ZeropageX : uint8 -> uint16 memory_mode
        | ZeropageY : uint8 -> uint16 memory_mode

    let contents (cpu : CPU.t) (type a) (mode : a memory_mode) : uint8 =
        match mode with
        | Accumulator -> cpu.accumulator
        | Absolute abs_addr -> CPU.fetch_ui8 cpu abs_addr
        | AbsoluteX abs_addr_x ->
            let incr_addr = abs_addr_x +++ !^(cpu.register_X) in
            CPU.fetch_ui8 cpu incr_addr
        | AbsoluteY abs_addr_y ->
            let incr_addr = abs_addr_y +++ !^(cpu.register_Y) in
            CPU.fetch_ui8 cpu incr_addr
        | Immediate b -> b
        | Indirect ind_addr -> CPU.fetch_ui16 cpu ind_addr |> CPU.fetch_ui8 cpu
        | XIndirect x_ind_addr ->
            let low_byte_targ =
                CPU.fetch_ui8 cpu !^(x_ind_addr ++ cpu.register_X)
            in
            let high_byte_targ =
                CPU.fetch_ui8 cpu !^(x_ind_addr ++ cpu.register_X ++ ~.0x01)
            in
            UInt16.combine_ui8 high_byte_targ low_byte_targ |> CPU.fetch_ui8 cpu
        | IndirectY ind_addr_y ->
            let low_byte_targ = CPU.fetch_ui8 cpu !^ind_addr_y in
            let high_byte_targ = CPU.fetch_ui8 cpu !^(ind_addr_y ++ ~.0x01) in
            UInt16.combine_ui8 high_byte_targ low_byte_targ +++ !^(cpu.register_Y)
            |> CPU.fetch_ui8 cpu
        | Relative b -> !^b +++ cpu.program_counter |> CPU.fetch_ui8 cpu
        | Zeropage zero_addr -> CPU.fetch_ui8 cpu !^zero_addr
        | ZeropageX zero_addr_x ->
            !^(zero_addr_x ++ cpu.register_X) |> CPU.fetch_ui8 cpu
        | ZeropageY zero_addr_y ->
            !^(zero_addr_y ++ cpu.register_Y) |> CPU.fetch_ui8 cpu

    let address (cpu : CPU.t) (type a) (mode : a memory_mode) : uint16 =
        match mode with
        | Absolute abs_addr -> abs_addr
        | AbsoluteX abs_addr_x -> abs_addr_x +++ !^(cpu.register_X)
        | AbsoluteY abs_addr_y -> abs_addr_y +++ !^(cpu.register_Y)
        | Indirect ind_addr -> CPU.fetch_ui16 cpu ind_addr
        | XIndirect x_ind_addr ->
            let low_byte_targ =
                CPU.fetch_ui8 cpu !^(x_ind_addr ++ cpu.register_X)
            in
            let high_byte_targ =
                CPU.fetch_ui8 cpu !^(x_ind_addr ++ cpu.register_X ++ ~.0x01)
            in
            UInt16.combine_ui8 high_byte_targ low_byte_targ
        | IndirectY ind_addr_y ->
            let low_byte_targ = CPU.fetch_ui8 cpu !^ind_addr_y in
            let high_byte_targ = CPU.fetch_ui8 cpu !^(ind_addr_y ++ ~.0x01) in
            UInt16.combine_ui8 high_byte_targ low_byte_targ +++ !^(cpu.register_Y)
        | Relative b -> ~^(~--.b + UInt16.to_int cpu.program_counter)
        | Zeropage zero_addr -> !^zero_addr
        | ZeropageX zero_addr_x -> !^(zero_addr_x ++ cpu.register_X)
        | ZeropageY zero_addr_y -> !^(zero_addr_y ++ cpu.register_Y)
        | _ -> raise (Failure "Memory mode incompatible with decode address")

    let add_unsigned_overflow (op_1 : uint8) (op_2 : uint8) : bool =
        if UInt8.to_int op_1 + UInt8.to_int op_2 > 0xFF then true else false

    let add_signed_overflow (op_1 : uint8) (op_2 : uint8) : bool =
        let signed_sum = ~--.op_1 + ~--.op_2 in
        if signed_sum > 127 || signed_sum < -128 then true else false

    let sub_unsigned_overflow (op_1 : uint8) (op_2 : uint8) : bool =
        if UInt8.to_int op_1 - UInt8.to_int op_2 < 0 then true else false

    let sub_signed_overflow (op_1 : uint8) (op_2 : uint8) : bool =
        let signed_diff = ~--.op_1 - ~--.op_2 in
        if signed_diff > 127 || signed_diff < -128 then true else false

    let incr_cpu_pc (cpu : CPU.t) (size : int) : CPU.t =
        { cpu with program_counter = cpu.program_counter +++ ~^size }

    let fetch_uint8_op (cpu : CPU.t) : uint8 =
        CPU.fetch_ui8 cpu cpu.program_counter

    let fetch_uint16_op (cpu : CPU.t) : uint16 =
        CPU.fetch_ui16 cpu cpu.program_counter
end
