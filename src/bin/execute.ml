(* open Ocamulator
open Ocamulator.Alias
open Graphics

(** [Execute.ml] contains code for executing ROM files. *)

(** [draw_screen cpu index] draws the current pixel at [index]  *)
let rec draw_screen (cpu : Cpu.t) (index : int) : unit =
    let cx = index mod Util.ssize * Util.psize in
    let cy = (Util.ssize - (index / Util.ssize)) * Util.psize in
    if index < Util.ssize * (Util.ssize + 1) then (
        Util.set_color (Cpu.fetch_ui8 cpu ~..(0x01E0 + index));
        Graphics.fill_rect cx cy Util.psize Util.psize;
        draw_screen cpu (index + 1))
    else ()

(** [step cpu] increments the game running in [cpu] by one step and updates
    the screen accordingly. *)
let rec step (cpu : Cpu.t) (n : int) : unit =
    if Graphics.key_pressed () then
        let key = Graphics.read_key () in
        let bit = ~.(Char.code key) in
        Cpu.write_ui8 cpu ~..0xFF bit
    else ();
    let opcode = Cpu.fetch_instruction cpu in
    let stepped = Opcode.step cpu opcode in
    let rand = ~.(Random.int 0xFE) ++ UInt8.one in
    let mn = if n > Util.stall then 0 else n + 1 in
    if mn > Util.stall then draw_screen stepped 0 else ();
    if mn > Util.stall then Cpu.write_ui8 stepped ~..0xFE rand else ();
    step stepped mn

(** [main ()] enters the ROM execution pipeline for this project. It will parse
    through the given ROM file, and display to the screen after every step. *)
let main () : unit =
    print_endline "\nThe available ROM files are:";
    let rom_files = Sys.readdir "../data/rom" in
    Array.iter (fun name -> print_endline name) rom_files;
    print_string "\nPlease enter the ROM file in ./data/rom to run: ";
    match read_line () with
    | exception End_of_file -> ()
    | file_name -> (
            try
                let channel = Util.open_rom file_name in
                let buffer = Bytes.make (0xFFFF + 1) '\x00' in
                let _ = In_channel.really_input_string channel 0x10 in
                let _ = In_channel.input channel buffer 0 0xFFFF in
                let cpu = Cpu.nes_cpu ~..0x0600 (Ram.nes_ram buffer) in
                let _ = Cpu.write_ui8 cpu ~..0xFE ~.0x2 in
                let _ = Graphics.open_graph "" in
                let size = Util.ssize * Util.psize in
                let _ = Graphics.resize_window size size in
                step cpu 0
            with
            | Graphic_failure "fatal I/O error" -> print_endline "Thanks for playing!"
            | Sys_error n -> print_endline "Please enter a valid \".nes\" file.")

(** Running main. *)
let _ = main () *)
