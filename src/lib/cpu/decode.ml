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
        | Absolute addr -> CPU.fetch_ui8 cpu addr
        | AbsoluteX addr ->
            let _addr = addr +++ !^(cpu.registerX) in
            CPU.fetch_ui8 cpu _addr
        | AbsoluteY addr ->
            let _addr = addr +++ !^(cpu.registerY) in
            CPU.fetch_ui8 cpu _addr
        | Immediate addr -> addr
        | Indirect addr ->
            let _addr = CPU.fetch_ui16 cpu addr in
            CPU.fetch_ui8 cpu _addr
        | XIndirect addr ->
            let low = CPU.fetch_ui8 cpu !^(addr ++ cpu.registerX) in
            let hig = CPU.fetch_ui8 cpu !^(addr ++ cpu.registerX ++ ~.0x01) in
            let _addr = UInt16.combine_ui8 hig low in
            CPU.fetch_ui8 cpu _addr
        | IndirectY addr ->
            let low = CPU.fetch_ui8 cpu !^addr in
            let hig = CPU.fetch_ui8 cpu !^(addr ++ ~.0x01) in
            let _addr = UInt16.combine_ui8 hig low +++ !^(cpu.registerY) in
            CPU.fetch_ui8 cpu _addr
        | Relative addr ->
            let _addr = !^addr +++ cpu.progCounter in
            CPU.fetch_ui8 cpu _addr
        | Zeropage addr ->
            let _addr = !^addr in
            CPU.fetch_ui8 cpu _addr
        | ZeropageX addr ->
            let _addr = !^(addr ++ cpu.registerX) in
            CPU.fetch_ui8 cpu _addr
        | ZeropageY addr ->
            let _addr = !^(addr ++ cpu.registerY) in
            CPU.fetch_ui8 cpu _addr

    let address (cpu : CPU.t) (type a) (mode : a memory_mode) : uint16 =
        match mode with
        | Absolute addr -> addr
        | AbsoluteX addr -> addr +++ !^(cpu.registerX)
        | AbsoluteY addr -> addr +++ !^(cpu.registerY)
        | Indirect addr -> CPU.fetch_ui16 cpu addr
        | XIndirect addr ->
            let low = CPU.fetch_ui8 cpu !^(addr ++ cpu.registerX) in
            let hig = CPU.fetch_ui8 cpu !^(addr ++ cpu.registerX ++ ~.0x01) in
            UInt16.combine_ui8 hig low
        | IndirectY addr ->
            let low = CPU.fetch_ui8 cpu !^addr in
            let hig = CPU.fetch_ui8 cpu !^(addr ++ ~.0x01) in
            UInt16.combine_ui8 hig low +++ !^(cpu.registerY)
        | Relative addr -> ~^(~% addr + UInt16.to_int cpu.progCounter)
        | Zeropage addr -> !^addr
        | ZeropageX addr -> !^(addr ++ cpu.registerX)
        | ZeropageY addr -> !^(addr ++ cpu.registerY)
        | _ -> raise (Failure "Invalid addressing mode")

    let fetch_ui8_op (cpu : CPU.t) : uint8 = CPU.fetch_ui8 cpu cpu.progCounter

    let increment_pc (cpu : CPU.t) (size : int) : CPU.t =
        { cpu with progCounter = cpu.progCounter +++ ~^size }

    let fetch_ui16_op (cpu : CPU.t) : uint16 = CPU.fetch_ui16 cpu cpu.progCounter
end
