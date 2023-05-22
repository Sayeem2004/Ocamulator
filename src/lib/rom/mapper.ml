open Alias

(* Specification found here: https://www.nesdev.org/wiki/NROM *)
let mapper_0 (chan : In_channel.t) (header : Header.t) (buffer : Bytes.t) =
  if header.prg_rom_size = ~.2 then
    match In_channel.really_input chan buffer 0x8000 0x8000 with
    | Some () -> ()
    | None -> failwith "Expected 32 KB of PRG ROM, but found less"
  else if header.prg_rom_size = ~.1 then
    let prg_rom_buffer = Bytes.make 0x4000 '\x00' in
    match In_channel.really_input chan prg_rom_buffer 0x0000 0x4000 with
    | Some () ->
      Bytes.blit prg_rom_buffer 0x0000 buffer 0x8000 0x4000;
      Bytes.blit prg_rom_buffer 0x0000 buffer 0xC000 0x4000
    | None -> failwith "Expected 16 KB of PRG ROM, but found less"
  else failwith "Invalid PRG ROM size for mapper 0"