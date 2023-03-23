open Lib__Cpu
open Lib__Ram
open Lib__UInt8
open Lib__UInt16

let step_cpu (cpu : CPU.t) : CPU.t =
    cpu

let rec step (cpu : CPU.t) : unit =
    print_endline "Step:";
    match read_line() with
    | exception End_of_file -> ()
    | command -> if (command = "Quit") then () else 
        let stepped_cpu = step_cpu (Opcode.step cpu) in
        step (stepped_cpu)

(** [main ()] enters the user interface pipeline for this project. It parses the
    ROM file specified by the first command line argument. It then initializes
    the CPU and RAM states. It processes the first couple instructions in the
    ROM file and displays the CPU and RAM state after every instruction. *)
let main () : unit =
    print_endline "Please enter the name of the ROM file in /data you would like to run:";
    match read_line () with
    | exception End_of_file -> ()
    | file_name -> 
        let nes_channel_in = open_in file_name in
        let nes_rom_buffer = Bytes.make 0xFF ' ' in
        let nes_rom = In_channel.input nes_channel_in nes_rom_buffer 0 0xFFFF in
        let nes_ram_rom = RAM.nes_ram nes_rom in
        let init_pc = RAM.read_ui16 nes_ram_rom (~^ 0xFFFC) in
        let new_nes_cpu = CPU.nes_cpu init_pc nes_ram_rom in


(** Running main. *)
let _ = main ()
