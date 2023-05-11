open Lib
open Lib.Alias

(** [read.ml] contains code for parsing ROM files. *)

(** [fetch_current_opcode cpu] is the opcode that was just run. *)
let fetch_current_opcode (cpu : Cpu.t) : uint8 = Cpu.fetch_instruction cpu

(** [pp_cpu cpu] is a pretty-printed string representing the Cpu state. *)
let pp_cpu (cpu : Cpu.t) : unit =
    let str8 = UInt8.to_string in
    let str16 = UInt16.to_string in
    let ptr = ~..0x01 +++ !**(cpu.stackPointer) in
    print_endline ("Cpu " ^ "{");
    print_endline ("    Accumulator: " ^ str8 cpu.accumulator);
    print_endline ("    Register X: " ^ str8 cpu.registerX);
    print_endline ("    Register Y: " ^ str8 cpu.registerY);
    print_endline ("    Program Counter: " ^ str16 cpu.progCounter);
    print_endline ("    Stack Pointer: " ^ str16 ptr);
    print_endline ("    RAM: " ^ "[ ... ... ... ]");
    print_endline ("    Flags: " ^ str8 (Cpu.flags_to_ui8 cpu.flags));
    print_endline ("}" ^ " ");
    print_endline("Current Instruction: " ^ str8 (fetch_current_opcode cpu))

(** [step_cpu_pc cpu] is the Cpu state after the program counter has been updated. *)
let step_cpu_pc (cpu : Cpu.t) : Cpu.t =
    { cpu with progCounter = cpu.progCounter +++ ~..1 }

(** [step cpu] runs an opcode and prints the Cpu state. *)
let rec step (cpu : Cpu.t) : unit =
    pp_cpu cpu;
    print_string "Step: ";
    match read_line () with
    | exception End_of_file -> ()
    | command -> (
            match command with
            | "quit" | "exit" -> ()
            | _ ->
                let opcode = fetch_current_opcode cpu in
                let stepped_cpu = Opcode.step cpu opcode in
                step stepped_cpu)

(** [main ()] enters the ROM parsing pipeline for this project. *)
let main () : unit =
    print_newline ();
    print_endline "Please enter the ROM file in ./data/rom to run:";
    match read_line () with
    | exception End_of_file -> ()
    | file_name ->
        let sep = Filename.dir_sep in
        let file_dir = "../data" ^ sep ^ "rom" ^ sep ^ file_name in
        let _ = print_endline file_dir in
        let nes_channel_in = open_in file_dir in
        let nes_rom_buffer = Bytes.make (0xFFFF + 1) '\x00' in
        let _ = In_channel.input nes_channel_in nes_rom_buffer 0 0xFFFF in
        let nes_ram_rom = Ram.nes_ram nes_rom_buffer in
        let init_pc = ~..0x10 in
        let new_nes_cpu = Cpu.nes_cpu init_pc nes_ram_rom in
        step new_nes_cpu

(** Running main. *)
let _ = main ()
