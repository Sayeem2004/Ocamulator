open Alias

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

let contents (cpu : Cpu.t) (type a) (mode : a memory_mode) : uint8 =
    match mode with
    | Accumulator -> cpu.accumulator
    | Absolute addr -> Cpu.fetch_ui8 cpu addr
    | AbsoluteX addr ->
        let addr = addr +++ !**(cpu.registerX) in
        Cpu.fetch_ui8 cpu addr
    | AbsoluteY addr ->
        let addr = addr +++ !**(cpu.registerY) in
        Cpu.fetch_ui8 cpu addr
    | Immediate addr -> addr
    | Indirect addr ->
        let addr = Cpu.fetch_ui16 cpu addr in
        Cpu.fetch_ui8 cpu addr
    | XIndirect addr ->
        let low = Cpu.fetch_ui8 cpu !**(addr ++ cpu.registerX) in
        let hig = Cpu.fetch_ui8 cpu !**(addr ++ cpu.registerX ++ ~.0x01) in
        let addr = !..hig low in
        Cpu.fetch_ui8 cpu addr
    | IndirectY addr ->
        let low = Cpu.fetch_ui8 cpu !**addr in
        let hig = Cpu.fetch_ui8 cpu !**(addr ++ ~.0x01) in
        let addr = !..hig low +++ !**(cpu.registerY) in
        Cpu.fetch_ui8 cpu addr
    | Relative addr ->
        let addr = !**addr +++ cpu.progCounter in
        Cpu.fetch_ui8 cpu addr
    | Zeropage addr ->
        let addr = !**addr in
        Cpu.fetch_ui8 cpu addr
    | ZeropageX addr ->
        let addr = !**(addr ++ cpu.registerX) in
        Cpu.fetch_ui8 cpu addr
    | ZeropageY addr ->
        let addr = !**(addr ++ cpu.registerY) in
        Cpu.fetch_ui8 cpu addr

let address (cpu : Cpu.t) (type a) (mode : a memory_mode) : uint16 =
    match mode with
    | Absolute addr -> addr
    | AbsoluteX addr -> addr +++ !**(cpu.registerX)
    | AbsoluteY addr -> addr +++ !**(cpu.registerY)
    | Indirect addr -> Cpu.fetch_ui16 cpu addr
    | XIndirect addr ->
        let low = Cpu.fetch_ui8 cpu !**(addr ++ cpu.registerX) in
        let hig = Cpu.fetch_ui8 cpu !**(addr ++ cpu.registerX ++ ~.0x01) in
        !..hig low
    | IndirectY addr ->
        let low = Cpu.fetch_ui8 cpu !**addr in
        let hig = Cpu.fetch_ui8 cpu !**(addr ++ ~.0x01) in
        !..hig low +++ !**(cpu.registerY)
    | Relative addr -> ~..(?%addr + ~**(cpu.progCounter))
    | Zeropage addr -> !**addr
    | ZeropageX addr -> !**(addr ++ cpu.registerX)
    | ZeropageY addr -> !**(addr ++ cpu.registerY)
    | _ -> raise (Failure "Invalid addressing mode")

let fetch_ui8_op (cpu : Cpu.t) : uint8 = Cpu.fetch_ui8 cpu cpu.progCounter

let increment_pc (cpu : Cpu.t) (size : int) : Cpu.t =
    { cpu with progCounter = cpu.progCounter +++ ~..size }

let fetch_ui16_op (cpu : Cpu.t) : uint16 = Cpu.fetch_ui16 cpu cpu.progCounter
