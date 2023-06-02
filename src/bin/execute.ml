open Ocamulator
open Ocamulator.Alias
open Graphics

module PpuBus = Bus.PpuBus (Nrom)
(** [Execute.ml] contains code for executing ROM files. *)

module Ppu = Ppu.Ppu (PpuBus)
module CpuBus = Bus.CpuBus (Nrom) (Ppu)
module Cpu = Cpu.Cpu (CpuBus)

let init_mem_pixel = 0x0000
let pixel_size = 10

let render_tile (ppu_bus : PpuBus.t) (bank : int) (tile_n : int) =
    let bank_ind = bank * 0x1000 in
    let start_tile_ind = bank_ind + (tile_n * 16) in
    for y = 7 downto 0 do
        let upper_addr = ~..(start_tile_ind + y) in
        let lower_addr = ~..(start_tile_ind + y + 8) in
        let upper = ref (PpuBus.read_ui8 ppu_bus upper_addr) in
        let lower = ref (PpuBus.read_ui8 ppu_bus lower_addr) in
        for x = 7 downto 0 do
            let value = ~.1 &&. !upper << 1 ||. (~.1 &&. !lower) in
            upper := !upper >> 1;
            lower := !lower >> 1;
            (match ~*value with
             | 0 -> Graphics.set_color Graphics.black
             | 1 -> Graphics.set_color (Graphics.rgb 80 80 80)
             | 2 -> Graphics.set_color (Graphics.rgb 140 140 140)
             | 3 -> Graphics.set_color (Graphics.rgb 200 200 200)
             | _ -> failwith "Bad\n   color");
            let screen_x = (x * 32) + (tile_n * 8 mod 32 * 32) in
            let screen_y = ((y * 32) + (tile_n * 8 / 32 * 32 * 8)) mod (32 * 8 * 3) in
            Graphics.fill_rect screen_x screen_y 32 32
        done
    done

(** [draw_screen cpu index] draws the current pixel at [index] *)
let rec draw_screen (cpu : Cpu.t) (index : int) : unit =
    let cx = index mod Util.ssize * Util.psize in
    let cy = (Util.ssize - (index / Util.ssize)) * Util.psize in
    if index < Util.ssize * (Util.ssize + 1) then (
        Util.set_color (Cpu.fetch_ui8 cpu ~..(init_mem_pixel + index));
        Graphics.fill_rect cx cy Util.psize Util.psize;
        draw_screen cpu (index + 1))
    else ()

let button_pressed (joypad : Joypad.t) (key : char) =
    match key with
    | 'd' -> Joypad.button_update joypad Right true
    | 'a' -> Joypad.button_update joypad Left true
    | 's' -> Joypad.button_update joypad Down true
    | 'w' -> Joypad.button_update joypad Up true
    | '1' -> Joypad.button_update joypad Start true
    | '2' -> Joypad.button_update joypad Select true
    | 'z' -> Joypad.button_update joypad Button_B true
    | 'x' -> Joypad.button_update joypad Button_A true
    | _ -> Joypad.reset_joypad joypad

let handle_user_input (joypad : Joypad.t) =
    if Graphics.key_pressed () then button_pressed joypad (Graphics.read_key ())
    else ()

(** [step cpu] increments the game running in [cpu] by one step and updates
    the screen accordingly. *)
let rec step (cpu : Cpu.t) (ppu : Ppu.t) (joypad : Joypad.t) (n : int) : unit =
    (* handle_user_input joypad; *)
    for i = 0 to 12 do
        Ppu.tick ppu
    done;
    step (Cpu.step cpu) ppu joypad 0
(* let _ = print_endline (Cpu.to_string cpu) in let _ = print_endline
   (Ppu.to_string ppu) in match Graphics.read_key () with | 'p' -> for i = 0 to
   1000 do Ppu.tick ppu done; step cpu ppu 0 | 'l' -> print_endline
   (CpuBus.to_string cpu.bus); step cpu ppu 0 | 'k' -> for i = 0 to 8 do
   Ppu.tick ppu done; step (Cpu.step cpu) ppu 0 | 'o' -> Ppu.tick ppu; step cpu
   ppu 0 | 'c' -> let super_stepped_cpu = ref cpu in for i = 0 to 0xFF do
   super_stepped_cpu := Cpu.step !super_stepped_cpu; print_endline
   (Cpu.to_string !super_stepped_cpu); done; step !super_stepped_cpu ppu 0 | 'r'
   -> Ppu.render ppu | 's' -> for i = 0 to 1000 do Ppu.tick ppu done; let
   super_stepped_cpu = ref cpu in for i = 0 to 332 do super_stepped_cpu :=
   Cpu.step !super_stepped_cpu; print_endline (Cpu.to_string
   !super_stepped_cpu); print_endline (Ppu.to_string ppu) done; step
   !super_stepped_cpu ppu 0 | _ -> let stepped_cpu = Cpu.step cpu in step
   stepped_cpu ppu 0 *)

(** [main ()] enters the ROM execution pipeline for this project. It will
    parse through the given ROM file, and display to the screen after every step.
*)
(* let main () : unit = print_endline "\nThe available ROM files are:"; let
   rom_files = Sys.readdir "../data/rom" in Array.iter (fun name ->
   print_endline name) rom_files; print_string "\nPlease enter the ROM file in\n
   ./data/rom to run: "; match read_line () with | exception End_of_file -> () |
   file_name -> ( try let channel = Util.open_rom file_name in let buffer =
   Bytes.make (0xFFFF + 1) '\x00' in let _ = In_channel.really_input_string
   channel 0x10 in let _ = In_channel.input channel buffer 0 0xFFFF in let cpu =
   Cpu.nes_cpu ~..0x0600 (Ram.nes_ram buffer) in let _ = Cpu.write_ui8 cpu
   ~..0xFE ~.0x2 in let _ = Graphics.open_graph "" in let size = Util.ssize *
   Util.psize in let _ = Graphics.resize_window size size in step cpu 0 with |
   Graphic_failure "fatal I/O error" -> print_endline "Thanks for playing!" |
   Sys_error n -> print_endline "Please enter a valid \".nes\" file.") *)

let left_joypad_addr = ~..0x4016
let right_joypad_addr = ~..0x4017

let test_main () : unit =
    print_endline "\nThe available ROM files are:";
    let rom_files = Sys.readdir "../data/rom" in
    Array.iter (fun name -> print_endline name) rom_files;
    print_string "\nPlease enter the ROM file in\n   ./data/rom to run: ";
    match read_line () with
    | exception End_of_file -> ()
    | file_name -> (
            try
                let channel = Util.open_rom file_name in
                let size = In_channel.length channel |> Int64.to_int in
                let buffer = Bytes.create size in
                let _ = In_channel.input channel buffer 0 size in
                let ines = Ines.cartridge_from_rom buffer in
                let nrom = Nrom.fresh ines in
                let ppu_bus = PpuBus.fresh nrom in
                let ppu = Ppu.fresh ppu_bus in
                let joypad = Joypad.fresh left_joypad_addr in
                let cpu_bus = CpuBus.fresh nrom ppu joypad in
                let pc_low = CpuBus.read_ui8 cpu_bus ~..0xFFFC in
                let pc_hi = CpuBus.read_ui8 cpu_bus ~..0xFFFD in
                let pc = !..pc_hi pc_low in
                let cpu = Cpu.spec_cpu pc ~.0xFD ~.0 ~.0 ~.0 ~.0x34 cpu_bus in
                let _ = Graphics.open_graph "" in
                let size = Util.ssize * Util.psize in
                let _ = Graphics.resize_window size size in
                let _ = Graphics.set_line_width 100 in
                step cpu ppu joypad 0
            with
            | Graphic_failure "fatal I/O error" -> print_endline "Thanks for playing!"
            | Sys_error n -> print_endline "Please enter a valid\n   \".nes\" file.")

(** Running main. *)
let _ = test_main ()
