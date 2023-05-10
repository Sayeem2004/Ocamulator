open Lib
open Lib.Alias
open Graphics

(** [main.ml] contains code for running the emulator. *)

(** [main ()] enters the main pipeline for this project. *)
let main () : unit =
    print_endline "";
    print_endline "Parsing:  Not Implemented...";
    print_endline "CPU:      Not Implemented...";
    print_endline "RAM:      Not Implemented...";
    print_endline "Graphics: Not Implemented..."

(** [fetch_current_opcode cpu] is the opcode that was just run. *)
let fetch_current_opcode (cpu : Cpu.t) : uint8 = Cpu.fetch_instruction cpu

(** [pp_cpu cpu] is a pretty-printed string representing the Cpu state. *)
let pp_cpu (cpu : Cpu.t) : unit =
    let str8 = UInt8.to_string in
    let str16 = UInt16.to_string in
    print_endline ("Cpu " ^ "{");
    print_endline ("    Accumulator: " ^ str8 cpu.accumulator);
    print_endline ("    Register X: " ^ str8 cpu.registerX);
    print_endline ("    Register Y: " ^ str8 cpu.registerY);
    print_endline ("    Program Counter: " ^ str16 cpu.progCounter);
    print_endline ("    Stack Pointer: " ^ str8 cpu.stackPointer);
    print_endline ("    RAM: " ^ "[ ... ... ... ]");
    print_endline ("    Flags: " ^ str8 (Cpu.flags_to_ui8 cpu.flags));
    print_endline ("}" ^ " ");
    print_endline("Current Instruction: " ^ str8 (fetch_current_opcode cpu))

let color_from_u8 (c : uint8) : unit =
    match ~* c with
    | 0x00 -> Graphics.set_color Graphics.black
    | 0x01 -> Graphics.set_color Graphics.white
    | 0x02 | 0x03 -> Graphics.set_color Graphics.red
    | 0x04 | 0x05 | 0x06 -> Graphics.set_color Graphics.green
    | 0x07 | 0x08 | 0x09 -> Graphics.set_color Graphics.blue
    | 0x0A | 0x0B -> Graphics.set_color Graphics.yellow
    | 0x0C | 0x0D -> Graphics.set_color Graphics.cyan
    | _ -> Graphics.set_color Graphics.magenta

let color_from_u8 (c : uint8) : unit = 
    match ~* c with
    | 0x0 -> Graphics.set_color Graphics.black
    | 0x1 -> Graphics.set_color Graphics.white
    | 0x2 -> Graphics.set_color Graphics.red
    | 0x3 -> Graphics.set_color Graphics.cyan
    | 0x4 -> Graphics.set_color (Graphics.rgb 128 0 128)
    | 0x5 -> Graphics.set_color Graphics.green
    | 0x6 -> Graphics.set_color Graphics.blue
    | 0x7 -> Graphics.set_color Graphics.yellow
    | 0x8 -> Graphics.set_color (Graphics.rgb 255 165 0)
    | 0x9 -> Graphics.set_color (Graphics.rgb 150 75 0)
    | 0xa -> Graphics.set_color (Graphics.rgb 255 69 0)
    | 0xb -> Graphics.set_color (Graphics.rgb 105 105 105)
    | 0xc -> Graphics.set_color (Graphics.rgb 169 169 169)
    | 0xd -> Graphics.set_color (Graphics.rgb 144 238 144)
    | 0xe -> Graphics.set_color (Graphics.rgb 173 216 230)
    | 0xf -> Graphics.set_color (Graphics.rgb 211 211 211)
    | _ -> Graphics.set_color Graphics.magenta

(* let color_from_u8 (c : uint8) : unit =
    match ~*c with
    | 0x00 *)

let pix_size = 32

let rec draw_screen_graphics (cpu : Cpu.t) (cur_index : int) : unit =
    let x = (cur_index mod 32) * pix_size in
    let y = (32 - cur_index / 32) * pix_size in
    if cur_index < 32 * 32 then
        let current_position_color = Cpu.fetch_ui8 cpu (~.. (0x01E0 + cur_index)) in
        color_from_u8 current_position_color;
        Graphics.fill_rect x y pix_size pix_size;
        draw_screen_graphics cpu (cur_index + 1)
    else ()

let rec draw_screen_terminal (cpu : Cpu.t) (cur_index : int) : unit = 
    if cur_index < 32 * 32 then
        let current_position_color = Cpu.fetch_ui8 cpu (~.. (0x200 + cur_index)) in
        match ~* current_position_color with
        | 0x00 -> print_string " "; if (cur_index mod 32 = 0) then print_endline ""; draw_screen_terminal (cpu) (cur_index + 1);
        | _ -> print_string "x"; if (cur_index mod 32 = 0) then print_endline ""; draw_screen_terminal (cpu) (cur_index + 1);
    else ()

(** [step cpu] runs the snake game on [cpu]. *)
let rec step_snake (cpu : Cpu.t) (draw : int) : unit =
    if Graphics.key_pressed () then
        let pressed_key = Graphics.read_key () in
        let pressed_key_bit = ~. (Char.code pressed_key) in
        Cpu.write_ui8 cpu (~.. 0xFF) pressed_key_bit;
    else ();
    let opcode = fetch_current_opcode cpu in
    let stepped_cpu = Opcode.step cpu opcode in
    let new_draw = if draw > 1200 then 0 else draw + 1 in
    if draw > 1200 then draw_screen_graphics stepped_cpu 0 else ();
    if draw > 1200 then Cpu.write_ui8 stepped_cpu (~.. 0xFE) (~. (Random.int 0xF) ++ UInt8.one) else ();
    step_snake stepped_cpu new_draw

let snake_main () : unit =
    print_newline ();
    print_endline "Please enter the ROM file in ./data/rom to run:";
    match read_line () with
    | exception End_of_file -> ()
    | file_name ->
        let sep = Filename.dir_sep in
        let snake_file_dir = "../data" ^ sep ^ "rom" ^ sep ^ file_name in
        let snake_rom_channel = open_in snake_file_dir in
        let nes_rom_buffer = Bytes.make (0xFFFF + 1) '\x00' in
        let _ = In_channel.really_input_string snake_rom_channel 0x10 in
        let _ = In_channel.input snake_rom_channel nes_rom_buffer 0 0xFFFF in
        let nes_ram_rom = Ram.nes_ram nes_rom_buffer in
        let init_pc = ~.. 0x0600 in
        let new_nes_cpu = Cpu.nes_cpu init_pc nes_ram_rom in
        Cpu.write_ui8 new_nes_cpu (~.. 0xFE) (~. 0x2);
        Graphics.open_graph "";
        Graphics.resize_window (32 * pix_size) (32 * pix_size);
        step_snake new_nes_cpu 0

(** Running main. *)
let _ = snake_main ()
