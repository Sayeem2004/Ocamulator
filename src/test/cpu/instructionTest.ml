open OUnit2
open Lib__UInt8
open Lib__UInt16
open Lib__Cpu
open Lib__Ram
open Lib__Opcode

(** [cpu_zero] is a CPU with an empty RAM array. *)
let cpu_zero : CPU.t = CPU.nes_cpu (UInt16.from_int 0) (RAM.zero_ram ())

(** [flag_true] is a flag type variable where all flags are true. *)
let flag_true : CPU.cpu_flags =
    {
        carry = true;
        zero = true;
        interrupt = true;
        decimal = true;
        break = true;
        overflow = true;
        negative = true;
        reserved = true;
    }

(** [cpu_true] is a CPU with all flags set to true. *)
let cpu_true : CPU.t =
    { (CPU.nes_cpu (UInt16.from_int 0) (RAM.zero_ram ())) with flags = flag_true }

(** [step_test cpu mode] asserts that [step cpu mode] does not raise an
    exception when run. *)
let step_test (name : string) (cpu : CPU.t) (opcode : uint8) : test =
    let _ = Opcode.step cpu opcode in
    name >:: fun _ -> assert_string ""

(** Instruction tests to be run. *)
let tests : test list =
    List.flatten
        [
            List.init 256 (fun i ->
                let opcode = UInt8.from_int i in
                let name =
                    Printf.sprintf "step cpu_zero opcode %s" (UInt8.to_string opcode)
                in
                step_test name cpu_zero opcode);
            List.init 256 (fun i ->
                let opcode = UInt8.from_int i in
                let name =
                    Printf.sprintf "step cpu_true opcode %s" (UInt8.to_string opcode)
                in
                step_test name cpu_true opcode);
        ]
