open Alias

(* For documentation on all of these flags and what they mean, see:
   https://www.nesdev.org/wiki/INES. *)

type flags_6 = {
    mirroring : bool;
    persistent_mem : bool;
    trainer_present : bool;
    ignore_mirroring_control : bool;
    lower_mapper_number : uint4;
}
(** mirroring: 0 for horizontal CIRAM A10 = PPU A11, 1 for vertical
    CIRAM A10 = PPU A10

    persistent_mem: If cartridge contains battery-backed PRG RAM ($6000 - $7FFF)
    or other persistent memory

    trainer_present: Whether there is 512-byte trainer at $7000 - $71FF, before
    PRG data

    ignore_mirroring_control: Ignore mirroring control and provide four-screen
    RAM
*)

(* Got lazy and didn't want to write specifications for other flags, see website
   at top for details *)

type flags_7 = {
    vs_unisystem : bool;
    playchoice : bool;
    nes_2 : bool;
    upper_mapper_number : uint4;
}

type flags_8 = { prg_ram_size : uint8 }

type tv_system =
    | NTSC
    | PAL
    | DUAL

type flags_9 = { tv_system : tv_system }

type flags_10 = {
    tv_system : tv_system;
    prg_ram_present : bool;
    bus_conflicts : bool;
}

type t = {
    prg_rom_size : uint8;
    chr_rom_size : uint8;
    flags_6 : flags_6;
    flags_7 : flags_7;
    flags_8 : flags_8;
    flags_9 : flags_9;
    flags_10 : flags_10;
}

let nes_const_string = "NES" ^ String.make 1 '\x1A'
let nes_const_byte = Bytes.of_string nes_const_string

let nes_const_ver (header_buffer : Bytes.t) =
    if Bytes.compare header_buffer nes_const_byte = 0 then ()
    else failwith "NES header missing initial constants"

let prg_rom_ver (header_buffer : Bytes.t) =
    let prg_rom_size = Bytes.get_uint8 header_buffer 0x4 in
    ~.prg_rom_size

let chr_rom_ver (header_buffer : Bytes.t) =
    let chr_rom_size = Bytes.get_uint8 header_buffer 0x5 in
    ~.chr_rom_size

let flags_6_ver (header_buffer : Bytes.t) =
    let flags_6_ui8 = Bytes.get_uint8 header_buffer 0x6 |> ( ~. ) in
    {
        mirroring = ?&flags_6_ui8 0;
        persistent_mem = ?&flags_6_ui8 1;
        trainer_present = ?&flags_6_ui8 2;
        ignore_mirroring_control = ?&flags_6_ui8 3;
        lower_mapper_number = UInt8.ui4_from_ui8 (flags_6_ui8 >> 4);
    }

let flags_7_ver (header_buffer : Bytes.t) =
    let flags_7_ui8 = Bytes.get_uint8 header_buffer 0x7 |> ( ~. ) in
    {
        vs_unisystem = ?&flags_7_ui8 0;
        playchoice = ?&flags_7_ui8 1;
        nes_2 = ?&flags_7_ui8 3 && not (?&flags_7_ui8 2);
        upper_mapper_number = UInt8.ui4_from_ui8 (flags_7_ui8 >> 4);
    }

let flags_8_ver (header_buffer : Bytes.t) =
    { prg_ram_size = Bytes.get_uint8 header_buffer 0x8 |> ( ~. ) }

let flags_9_ver (header_buffer : Bytes.t) =
    let flags_9_ui8 = Bytes.get_uint8 header_buffer 0x9 |> ( ~. ) in
    let tv_system_bit = ?&flags_9_ui8 0 in
    let reserved = flags_9_ui8 >> 1 in
    if reserved <-> UInt8.zero then
        { tv_system = (if tv_system_bit then PAL else NTSC) }
    else failwith "Reserved bits of flag 9 set"

let flags_10_ver (header_buffer : Bytes.t) =
    let flags_10_ui8 = Bytes.get_uint8 header_buffer 0xA |> ( ~. ) in
    let tv_system =
        match (?&flags_10_ui8 1, ?&flags_10_ui8 0) with
        | false, false -> NTSC
        | true, false -> PAL
        | _ -> DUAL
    in
    {
        tv_system;
        prg_ram_present = ?&flags_10_ui8 0x4;
        bus_conflicts = ?&flags_10_ui8 0x5;
    }

let header_from_rom_chan (chan : In_channel.t) =
    let header_buffer = Bytes.make 0xF '\x00' in
    match In_channel.really_input chan header_buffer 0 0xF with
    | Some () ->
        nes_const_ver header_buffer;
        {
            prg_rom_size = prg_rom_ver header_buffer;
            chr_rom_size = chr_rom_ver header_buffer;
            flags_6 = flags_6_ver header_buffer;
            flags_7 = flags_7_ver header_buffer;
            flags_8 = flags_8_ver header_buffer;
            flags_9 = flags_9_ver header_buffer;
            flags_10 = flags_10_ver header_buffer;
        }
    | None -> failwith "Invalid size for NES header"
