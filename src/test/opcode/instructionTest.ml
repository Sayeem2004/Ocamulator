open Ocamulator
open OUnit2
open Ocamulator.Alias

(** [InstructionTest.ml] contains coverage and accuracy tests for [Instruction.ml]. *)

(** [cpu_zero] is a Cpu with an empty RAM array. *)
let cpu_zero : Cpu.t = Cpu.nes_cpu ~..0 (Ram.zero_ram ())

(** [flag_true] is a flag type variable where all flags are true. *)
let flag_true : Cpu.flags =
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

(** [cpu_true] is a Cpu with all flags set to true. *)
let cpu_true : Cpu.t =
    { (Cpu.nes_cpu ~..0 (Ram.zero_ram ())) with flags = flag_true }

(** [step_test cpu mode] asserts that [step cpu mode] does not raise an
    exception when run. *)
let step_test (name : string) (cpu : Cpu.t) (opcode : uint8) : test =
    let _ = Opcode.step cpu opcode in
    name >:: fun _ -> assert_string ""

(** Instruction tests to be run. *)
let tests : test list =
    List.flatten
        [
            List.init 256 (fun i ->
                let name = Printf.sprintf "cpu_zero %s" (UInt8.to_string ~.i) in
                step_test name cpu_zero ~.i);
            List.init 256 (fun i ->
                let name = Printf.sprintf "cpu_true %s" (UInt8.to_string ~.i) in
                step_test name cpu_true ~.i);
        ]
