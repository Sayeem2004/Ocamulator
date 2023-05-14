open Ocamulator
open Ocamulator.Alias

(** [Simulate.ml] contains code for simulating ROM files. *)

(** [print_prompt cpu] prints the CPU state, current instruction, and user prompt. *)
let print_prompt (cpu : Cpu.t) : unit =
    let cpu_state = Cpu.to_string cpu ^ " " in
    let curr_inst = UInt8.to_string (Cpu.fetch_instruction cpu) ^ "\n" in
    let user_inst = "Press enter to step, or type 'quit' to exit: " in
    print_string (cpu_state ^ curr_inst ^ user_inst)

(** [step cpu] prints the total prompt and steps the CPU. *)
let rec step (cpu : Cpu.t) : unit =
    print_prompt cpu;
    match read_line () with
    | exception End_of_file -> ()
    | command -> (
            match command with
            | "quit" | "exit" -> ()
            | _ -> Opcode.step cpu (Cpu.fetch_instruction cpu) |> step)

(** [main ()] enters the ROM simulation pipeline for this project. It will parse
    through the given ROM file, and display the CPU state after every step. *)
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
                let _ = In_channel.input channel buffer 0 0xFFFF in
                let _ = In_channel.close channel in
                let cpu = Cpu.nes_cpu ~..0x10 (Ram.nes_ram buffer) in
                step cpu
            with Sys_error n -> print_endline "Please enter a valid \".nes\" file.")

(** Running main. *)
let _ = main ()
