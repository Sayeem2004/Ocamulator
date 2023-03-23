open Lib__Cpu
open Lib__Ram
open Lib__UInt8
open Lib__UInt16
open Lib__Opcode

let pp_cpu (cpu : CPU.t) : unit =
    print_endline "CPU {";
    print_endline ("    Accumulator: " ^ UInt8.to_string cpu.accumulator);
    print_endline ("    Register X: " ^ UInt8.to_string cpu.register_X);
    print_endline ("    Register Y: " ^ UInt8.to_string cpu.register_Y);
    print_endline ("    Program Counter: " ^ UInt8.to_string cpu.register_Y);
    print_endline ("    Stack Pointer: " ^ UInt16.to_string (~^ 0x01 +++ !^ (cpu.stack_pointer)));
    print_endline ("    Flags: " ^ UInt8.to_string (CPU.flags_ui8 cpu));
    print_endline "}"

let fetch_current_opcode (cpu : CPU.t) : uint8 =
    CPU.fetch_current_instruction cpu

let step_cpu_pc (cpu : CPU.t) : CPU.t =
    {cpu with program_counter = cpu.program_counter +++ ~^ 1}

let rec step (cpu : CPU.t) : unit =
    pp_cpu cpu;
    print_endline "Step:";
    match read_line() with
    | exception End_of_file -> ()
    | command -> if (command = "Quit") then () else
        let opcode = fetch_current_opcode cpu in
        let cpu_stepped_pc = step_cpu_pc cpu in
        let stepped_cpu = Opcode.step cpu_stepped_pc opcode in
        step (stepped_cpu)

(**)
(** [main ()] enters the user interface pipeline for this project. It parses the
    ROM file specified by the first command line argument. It then initializes
    the CPU and RAM states. It processes the first couple instructions in the
    ROM file and displays the CPU and RAM state after every instruction. *)
let main () : unit =
    print_newline ();
    print_endline "Please enter the name of the ROM file in /data you would like to run:";
    match read_line () with
    | exception End_of_file -> ()
    | file_name ->
        let file_dir = "data" ^ Filename.dir_sep ^ file_name in
        let _ = print_endline file_dir in
        let nes_channel_in = open_in file_dir in
        let nes_rom_buffer = Bytes.make 0xFFFF ' ' in
        let _ = In_channel.input nes_channel_in nes_rom_buffer 0 0xFFFF in
        let nes_ram_rom = RAM.nes_ram nes_rom_buffer in
        let init_pc = RAM.read_ui16 nes_ram_rom (~^ 0xFFFC) in
        let new_nes_cpu = CPU.nes_cpu init_pc nes_ram_rom in
        step new_nes_cpu

(** Running main. *)
let _ = main ()