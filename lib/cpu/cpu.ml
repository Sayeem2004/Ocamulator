open Lib__UInt8
open Lib__UInt16
open Lib__Ram

module CPU = struct
    type flag =
    | Carry
    | Zero
    | Interrupt
    | Decimal
    | Break
    | Reserved
    | Overflow
    | Sign

    type t = {
        accumulator: uint8;
        register_X: uint8;
        register_Y: uint8;
        program_counter: uint16;
        ram: RAM.t;
        flags: uint8;
    };;

    let carr_uint8 (cpu : t) : uint8 =
        UInt8.logand cpu.flags (UInt8.from_int 0b00000001)

    let flag_on (cpu : t) (fl : flag) =
        match fl with
        | Carry -> {cpu with flags = UInt8.logor cpu.flags (UInt8.from_int 0b00000001)}
        | Zero -> {cpu with flags = UInt8.logor cpu.flags (UInt8.from_int 0b00000010)}
        | Interrupt -> {cpu with flags = UInt8.logor cpu.flags (UInt8.from_int 0b00000100)}
        | Decimal -> {cpu with flags = UInt8.logor cpu.flags (UInt8.from_int 0b00001000)}
        | Break -> {cpu with flags = UInt8.logor cpu.flags (UInt8.from_int 0b00010000)}
        | Reserved -> {cpu with flags = UInt8.logor cpu.flags (UInt8.from_int 0b00100000)}
        | Overflow -> {cpu with flags = UInt8.logor cpu.flags (UInt8.from_int 0b01000000)}
        | Sign -> {cpu with flags = UInt8.logor cpu.flags (UInt8.from_int 0b10000000)}

    let flag_off (cpu : t) (fl : flag) =
        match fl with
        | Carry -> {cpu with flags = UInt8.logand cpu.flags (UInt8.from_int 0b11111110)}
        | Zero -> {cpu with flags = UInt8.logand cpu.flags (UInt8.from_int 0b11111101)}
        | Interrupt -> {cpu with flags = UInt8.logand cpu.flags (UInt8.from_int 0b11111011)}
        | Decimal -> {cpu with flags = UInt8.logand cpu.flags (UInt8.from_int 0b11110111)}
        | Break -> {cpu with flags = UInt8.logand cpu.flags (UInt8.from_int 0b11101111)}
        | Reserved -> {cpu with flags = UInt8.logand cpu.flags (UInt8.from_int 0b11011111)}
        | Overflow -> {cpu with flags = UInt8.logand cpu.flags (UInt8.from_int 0b10111111)}
        | Sign -> {cpu with flags = UInt8.logand cpu.flags (UInt8.from_int 0b01111111)}

    let flag_on (cpu : t) (fl : flag) =
        match fl with
        | Carry -> {cpu with flags = UInt8.logxor cpu.flags (UInt8.from_int 0b00000001)}
        | Zero -> {cpu with flags = UInt8.logxor cpu.flags (UInt8.from_int 0b00000010)}
        | Interrupt -> {cpu with flags = UInt8.logxor cpu.flags (UInt8.from_int 0b00000100)}
        | Decimal -> {cpu with flags = UInt8.logxor cpu.flags (UInt8.from_int 0b00001000)}
        | Break -> {cpu with flags = UInt8.logxor cpu.flags (UInt8.from_int 0b00010000)}
        | Reserved -> {cpu with flags = UInt8.logxor cpu.flags (UInt8.from_int 0b00100000)}
        | Overflow -> {cpu with flags = UInt8.logxor cpu.flags (UInt8.from_int 0b01000000)}
        | Sign -> {cpu with flags = UInt8.logxor cpu.flags (UInt8.from_int 0b10000000)}

    let fetch_ui8 (cpu: t) (addr: uint16) : uint8 = RAM.read_ui8 cpu.ram addr;;
    let fetch_ui16 (cpu: t) (addr: uint16) : uint16 = RAM.read_ui16 cpu.ram addr;;
    (* let write_ui8 (cpu : t) (addr : uint16) (value : uint8) = RAM.write_ui8 cpu.ram addr value;;
    let write_ui16 (cpu : t) (addr : uint16) (value : uint16) = RAM.write_ui16 cpu.ram addr value;; *)
end
