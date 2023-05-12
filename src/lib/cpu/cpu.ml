open Alias

type flags = {
    negative : bool;
    overflow : bool;
    reserved : bool;
    break : bool;
    decimal : bool;
    interrupt : bool;
    zero : bool;
    carry : bool;
}

type t = {
    accumulator : uint8;
    registerX : uint8;
    registerY : uint8;
    progCounter : uint16;
    stackPointer : uint8;
    flags : flags;
    ram : Ram.t;
}

let flags_to_ui8 (flags : flags) : uint8 =
    let ngtv = ~.0 ++ ?.(flags.negative) << 1 in
    let over = ngtv ++ ?.(flags.overflow) << 1 in
    let resv = over ++ ?.(flags.reserved) << 1 in
    let brek = resv ++ ?.(flags.break) << 1 in
    let decm = brek ++ ?.(flags.decimal) << 1 in
    let intr = decm ++ ?.(flags.interrupt) << 1 in
    let zero = intr ++ ?.(flags.zero) << 1 in
    zero ++ ?.(flags.carry)

let flags_from_ui8 (flags : uint8) : flags =
    {
        negative = not (flags &&. ~.0b10000000 <-> ~.0x00);
        overflow = not (flags &&. ~.0b01000000 <-> ~.0x00);
        reserved = not (flags &&. ~.0b00100000 <-> ~.0x00);
        break = not (flags &&. ~.0b00010000 <-> ~.0x00);
        decimal = not (flags &&. ~.0b00001000 <-> ~.0x00);
        interrupt = not (flags &&. ~.0b00000100 <-> ~.0x00);
        zero = not (flags &&. ~.0b00000010 <-> ~.0x00);
        carry = not (flags &&. ~.0b00000001 <-> ~.0x00);
    }

let nes_cpu (pc : uint16) (ram : Ram.t) : t =
    {
        accumulator = ~.0;
        registerX = ~.0;
        registerY = ~.0;
        progCounter = pc;
        stackPointer = ~.0xFF;
        ram;
        flags =
            {
                carry = false;
                zero = false;
                interrupt = false;
                decimal = false;
                negative = false;
                overflow = false;
                break = true;
                reserved = true;
            };
    }

let spec_cpu (pc : uint16) (sp : uint8) (acc : uint8) (regX : uint8)
        (regY : uint8) (flags : uint8) (ram : Ram.t) : t =
    {
        accumulator = acc;
        registerX = regX;
        registerY = regY;
        progCounter = pc;
        stackPointer = sp;
        ram;
        flags = flags_from_ui8 flags;
    }

let fetch_ui8 (cpu : t) (addr : uint16) : uint8 = Ram.read_ui8 cpu.ram addr

let fetch_ui16 (cpu : t) (addr : uint16) : uint16 =
    let lo = fetch_ui8 cpu addr in
    let hi = fetch_ui8 cpu (addr +++ ~..0x01) in
    !..hi lo

let fetch_instruction (cpu : t) : uint8 = fetch_ui8 cpu cpu.progCounter

let write_ui8 (cpu : t) (addr : uint16) (value : uint8) : unit =
    Ram.write_ui8 cpu.ram addr value

let absolute_stack (cpu : t) : uint16 = ~..0x0100 +++ !**(cpu.stackPointer)

let push_stack_ui8 (cpu : t) (value : uint8) : t =
    let stack_loc = absolute_stack cpu in
    write_ui8 cpu stack_loc value;
    { cpu with stackPointer = cpu.stackPointer -- ~.0x01 }

let push_stack_ui16 (cpu : t) (value : uint16) : t =
    let hi, lo = !@@value in
    let pushed_cpu = push_stack_ui8 cpu hi in
    push_stack_ui8 pushed_cpu lo

let pop_stack_ui8 (cpu : t) : t =
    { cpu with stackPointer = cpu.stackPointer ++ ~.0x01 }

let pop_stack_ui16 (cpu : t) : t =
    let pop_cpu = pop_stack_ui8 cpu in
    pop_stack_ui8 pop_cpu

let peek_stack_ui8 (cpu : t) : uint8 =
    let stack_loc = absolute_stack cpu in
    fetch_ui8 cpu (stack_loc +++ ~..0x01)

let peek_stack_ui16 (cpu : t) : uint16 =
    let lo = peek_stack_ui8 cpu in
    let pop_cpu = pop_stack_ui8 cpu in
    let hi = peek_stack_ui8 pop_cpu in
    !..hi lo

let to_string (cpu : t) : string =
    "Cpu {\n" ^ "\tAccumulator: "
    ^ UInt8.to_string cpu.accumulator
    ^ "\n" ^ "\tRegister X: "
    ^ UInt8.to_string cpu.registerX
    ^ "\n" ^ "\tRegister Y: "
    ^ UInt8.to_string cpu.registerY
    ^ "\n" ^ "\tProgram Counter: "
    ^ UInt16.to_string cpu.progCounter
    ^ "\n" ^ "\tStack Pointer: "
    ^ UInt8.to_string cpu.stackPointer
    ^ "\n" ^ "\tRAM: [ ... ... ... ]\n" ^ "\tFlags: "
    ^ UInt8.to_string (flags_to_ui8 cpu.flags)
    ^ "\n" ^ "}"
