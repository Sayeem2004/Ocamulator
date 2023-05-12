open Lib
open OUnit2
open Lib.Alias

(** [CpuTest.ml] contains coverage and accuracy tests for [Cpu.ml]. *)

(** [cpu_zero] is a Cpu with an empty RAM array. *)
let cpu_zero : Cpu.t = Cpu.nes_cpu ~..0 (Ram.zero_ram ())

(** [cpu_ui8] is a Cpu with a RAM array with value 128 set at position 0. *)
let cpu_ui8 : Cpu.t =
    let cpu = Cpu.nes_cpu ~..0 (Ram.zero_ram ()) in
    Cpu.write_ui8 cpu ~..0 ~.128;
    cpu

(** [flags_to_ui8_test name cpu exp] tests equivalence between [flags_to_ui8 cpu] and
    [exp]. *)
let flags_to_ui8_test (name : string) (cpu : Cpu.t) (exp : uint8) : test =
    name >:: fun _ ->
        assert_equal exp (Cpu.flags_to_ui8 cpu.flags) ~printer:UInt8.to_string

(** Flags_ui8 tests to be run. *)
let flags_to_ui8_tests : test list =
    [
        flags_to_ui8_test "flags_to_ui8_test_zero" cpu_zero ~.0x30;
        flags_to_ui8_test "flags_to_ui8_test_ui8" cpu_ui8 ~.0x30;
    ]

(** [fetch_ui8_test name cpu addr exp] tests equivalence between [fetch_ui8 cpu
    addr] and [exp]. *)
let fetch_ui8_test (name : string) (cpu : Cpu.t) (addr : uint16) (exp : uint8) :
    test =
    name >:: fun _ ->
        assert_equal exp (Cpu.fetch_ui8 cpu addr) ~printer:UInt8.to_string

(** Fetch_ui8 tests to be run. *)
let fetch_ui8_tests : test list =
    [
        fetch_ui8_test "fetch_ui8_test_zero" cpu_zero ~..0 ~.0;
        fetch_ui8_test "fetch_ui8_test_ui8" cpu_ui8 ~..0 ~.128;
    ]

(** [fetch_ui16_test name cpu addr exp] tests equivalence between [fetch_ui16
    cpu addr] and [exp]. *)
let fetch_ui16_test (name : string) (cpu : Cpu.t) (addr : uint16) (exp : uint16)
    : test =
    name >:: fun _ ->
        assert_equal exp (Cpu.fetch_ui16 cpu addr) ~printer:UInt16.to_string

(** Fetch_ui16 tests to be run. *)
let fetch_ui16_tests : test list =
    [
        fetch_ui16_test "fetch_ui16_test_zero" cpu_zero ~..0 ~..0;
        fetch_ui16_test "fetch_ui16_test_ui8" cpu_ui8 ~..0 ~..128;
    ]

(** [fetch_instruction_test name cpu exp] tests equivalence between
    [fetch_instruction cpu] and [exp]. *)
let fetch_instruction_test (name : string) (cpu : Cpu.t) (exp : uint8) : test =
    name >:: fun _ ->
        assert_equal exp (Cpu.fetch_instruction cpu) ~printer:UInt8.to_string

(** Fetch_current_instruction tests to be run. *)
let fetch_instruction_tests : test list =
    [
        fetch_instruction_test "fetch_instruction_test_zero" cpu_zero ~.0;
        fetch_instruction_test "fetch_instruction_test_ui8" cpu_ui8 ~.0x80;
    ]

(** [absolute_stack_test name cpu exp] tests equivalence between
    [absolute_stack cpu] and [exp]. *)
let absolute_stack_test (name : string) (cpu : Cpu.t) (exp : uint16) : test =
    name >:: fun _ ->
        assert_equal exp (Cpu.absolute_stack cpu) ~printer:UInt16.to_string

(** Absolute_loc_stack tests to be run. *)
let absolute_stack_tests : test list =
    [
        absolute_stack_test "absolute_stack_test_zero" cpu_zero ~..0x01FF;
        absolute_stack_test "absolute_stack_test_ui8" cpu_ui8 ~..0x01FF;
    ]

(** [push_stack_ui8_test name cpu v exp] tests equivalence between [push_stack_ui8
    cpu v] and [exp]. *)
let push_stack_ui8_test (name : string) (cpu : Cpu.t) (v : uint8) (exp : uint8)
    : test =
    name >:: fun _ ->
        let pushed_cpu = Cpu.push_stack_ui8 cpu v in
        assert_equal exp (Cpu.peek_stack_ui8 pushed_cpu) ~printer:UInt8.to_string

(** Push_stack_ui8 tests to be run. *)
let push_stack_ui8_tests : test list =
    [
        push_stack_ui8_test "push_stack_ui8_test_zero" cpu_zero ~.0x01 ~.0x01;
        push_stack_ui8_test "push_stack_ui8_test_ui8" cpu_ui8 ~.0x01 ~.0x01;
    ]

(** [push_stack_ui16_test name cpu v exp] tests equivalence between [push_stack_ui16
    cpu v] and [exp]. *)
let push_stack_ui16_test (name : string) (cpu : Cpu.t) (v : uint16)
        (exp : uint16) : test =
    name >:: fun _ ->
        let pushed_cpu = Cpu.push_stack_ui16 cpu v in
        assert_equal exp (Cpu.peek_stack_ui16 pushed_cpu) ~printer:UInt16.to_string

(** Push_stack_ui16 tests to be run. *)
let push_stack_ui16_tests : test list =
    [
        push_stack_ui16_test "push_stack_ui16_test_zero" cpu_zero ~..0x01 ~..0x01;
        push_stack_ui16_test "push_stack_ui16_test_ui8" cpu_ui8 ~..0x01 ~..0x01;
    ]

(** [pop_stack_test name cpu exp] tests equivalence between [pop_stack (push_stack
    (push_stack cpu v) v)] and [exp]. *)
let pop_stack_test (name : string) (cpu : Cpu.t) (v : uint8) (exp : uint8) :
    test =
    name >:: fun _ ->
        let pushed_1_cpu = Cpu.push_stack_ui8 cpu v in
        let pushed_2_cpu = Cpu.push_stack_ui8 pushed_1_cpu v in
        let popped_cpu = Cpu.pop_stack_ui8 pushed_2_cpu in
        assert_equal exp (Cpu.peek_stack_ui8 popped_cpu) ~printer:UInt8.to_string

(** Pop_stack tests to be run. *)
let pop_stack_tests : test list =
    [
        pop_stack_test "pop_stack_test_zero" cpu_zero ~.0x01 ~.0x01;
        pop_stack_test "pop_stack_test_ui8" cpu_ui8 ~.0x01 ~.0x01;
    ]

(** [to_string_test cpu exp] tests equivalence between [to_string cpu] and [exp]. *)
let to_string_test (name : string) (cpu : Cpu.t) (exp : string) : test =
    name >:: fun _ -> assert_equal exp (Cpu.to_string cpu) ~printer:(fun x -> x)

(** To_string tests to be run. *)
let to_string_tests : test list =
    [
        to_string_test "to_string_test_zero" cpu_zero
            "Cpu {\n\
             \tAccumulator: $0000\n\
             \tRegister X: $0000\n\
             \tRegister Y: $0000\n\
             \tProgram Counter: $0000\n\
             \tStack Pointer: $00FF\n\
             \tRAM: [ ... ... ... ]\n\
             \tFlags: $0030\n\
             }";
        to_string_test "to_string_test_ui8" cpu_ui8
            "Cpu {\n\
             \tAccumulator: $0000\n\
             \tRegister X: $0000\n\
             \tRegister Y: $0000\n\
             \tProgram Counter: $0000\n\
             \tStack Pointer: $00FF\n\
             \tRAM: [ ... ... ... ]\n\
             \tFlags: $0030\n\
             }";
    ]

(** Cpu tests to be run. *)
let tests : test list =
    List.flatten
        [
            flags_to_ui8_tests;
            fetch_ui8_tests;
            fetch_ui16_tests;
            fetch_instruction_tests;
            absolute_stack_tests;
            push_stack_ui8_tests;
            push_stack_ui16_tests;
            pop_stack_tests;
            to_string_tests;
        ]
