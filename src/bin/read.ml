open Lib__Cpu
open Lib__Ram
open Lib__UInt8
open Lib__Opcode
open Lib__UInt16

(** [pp_cpu cpu] is a pretty-printed string representing the CPU state. *)
let pp_cpu (cpu : CPU.t) : unit =
    let str8 = UInt8.to_string in
    let str16 = UInt16.to_string in
    let ptr = ~^0x01 +++ !^(cpu.stackPointer) in
    print_endline ("CPU " ^ "{");
    print_endline ("    Accumulator: " ^ str8 cpu.accumulator);
    print_endline ("    Register X: " ^ str8 cpu.registerX);
    print_endline ("    Register Y: " ^ str8 cpu.registerY);
    print_endline ("    Program Counter: " ^ str16 cpu.progCounter);
    print_endline ("    Stack Pointer: " ^ str16 ptr);
    print_endline ("    RAM: " ^ "[ ... ... ... ]");
    print_endline ("    Flags: " ^ str8 (CPU.flags_to_ui8 cpu.flags));
    print_endline ("}" ^ " ")

(** [fetch_current_opcode cpu] is the opcode that was just run. *)
let fetch_current_opcode (cpu : CPU.t) : uint8 = CPU.fetch_instruction cpu

(** [step_cpu_pc cpu] is the CPU state after the program counter has been updated. *)
let step_cpu_pc (cpu : CPU.t) : CPU.t =
    { cpu with progCounter = cpu.progCounter +++ ~^1 }

(** [step cpu] runs an opcode and prints the CPU state. *)
let rec step (cpu : CPU.t) : unit =
    pp_cpu cpu;
    print_string "Step: ";
    match read_line () with
    | exception End_of_file -> ()
    | command -> (
            match command with
            | "quit" | "exit" -> ()
            | _ ->
                let opcode = fetch_current_opcode cpu in
                let cpu_stepped_pc = step_cpu_pc cpu in
                let stepped_cpu = Opcode.step cpu_stepped_pc opcode in
                step stepped_cpu)

(** [main ()] enters the user interface pipeline for this project. It parses the
    ROM file specified by the first command line argument. It then initializes
    the CPU and RAM states. It processes the first couple instructions in the
    ROM file and displays the CPU and RAM state after every instruction. *)
let main () : unit =
    print_newline ();
    print_endline "Please enter the ROM file in ./data/rom to run:";
    match read_line () with
    | exception End_of_file -> ()
    | file_name ->
        let sep = Filename.dir_sep in
        let file_dir = "data" ^ sep ^ "rom" ^ sep ^ file_name in
        let _ = print_endline file_dir in
        let nes_channel_in = open_in file_dir in
        let nes_rom_buffer = Bytes.make (0xFFFF + 1) ' ' in
        let _ = In_channel.input nes_channel_in nes_rom_buffer 0 0xFFFF in
        let nes_ram_rom = RAM.nes_ram nes_rom_buffer in
        let init_pc = ~^0x10 in
        let new_nes_cpu = CPU.nes_cpu init_pc nes_ram_rom in
        step new_nes_cpu

(** Running main. *)
let _ = main ()
