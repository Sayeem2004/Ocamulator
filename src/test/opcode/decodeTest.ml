open Lib
open OUnit2
open Lib.Alias

(** [DecodeTest.ml] contains coverage and accuracy tests for [Decode.ml]. *)

(** [cpu_zero] is a Cpu with an empty RAM array. *)
let cpu_zero : Cpu.t = Cpu.nes_cpu ~..0 (Ram.zero_ram ())

(** [cpu_ui8] is a Cpu with a RAM array with value 128 set at position 0. *)
let cpu_ui8 : Cpu.t =
    let cpu = Cpu.nes_cpu ~..0 (Ram.zero_ram ()) in
    Cpu.write_ui8 cpu ~..0 ~.128;
    cpu

(** [contents_test name cpu mode exp] asserts the equivalence between [contents
    cpu mode] and [exp]. *)
let contents_test (name : string) (cpu : Cpu.t) (mode : 'a Decode.memory_mode)
        (exp : uint8) : test =
    name >:: fun _ ->
        assert_equal exp (Decode.contents cpu mode) ~printer:UInt8.to_string

(** Contents tests to be run. *)
let contents_tests : test list =
    [
        contents_test "contents accumulator" cpu_zero Accumulator ~.0;
        contents_test "contents absolute" cpu_zero (Absolute ~..0) ~.0;
        contents_test "contents absolute x" cpu_zero (AbsoluteX ~..0) ~.0;
        contents_test "contents absolute y" cpu_zero (AbsoluteY ~..0) ~.0;
        contents_test "contents immediate" cpu_zero (Immediate ~.0) ~.0;
        contents_test "contents indirect" cpu_zero (Indirect ~..0) ~.0;
        contents_test "contents x indirect" cpu_zero (XIndirect ~.0) ~.0;
        contents_test "contents indirect y" cpu_zero (IndirectY ~.0) ~.0;
        contents_test "contents relative" cpu_zero (Relative ~.0) ~.0;
        contents_test "contents zero page" cpu_zero (Zeropage ~.0) ~.0;
        contents_test "contents zero page x" cpu_zero (ZeropageX ~.0) ~.0;
        contents_test "contents zero page y" cpu_zero (ZeropageY ~.0) ~.0;
    ]

(** [address_test name cpu mode exp] asserts the equivalence between [address cpu
    mode] and [exp]. *)
let address_test (name : string) (cpu : Cpu.t) (mode : 'a Decode.memory_mode)
        (exp : uint16) : test =
    name >:: fun _ ->
        assert_equal exp (Decode.address cpu mode) ~printer:UInt16.to_string

(** [address_raise_test name cpu mode] asserts that [address cpu mode] raises an
    exception. *)
let address_raise_test (name : string) (cpu : Cpu.t)
        (mode : 'a Decode.memory_mode) : test =
    name >:: fun _ ->
        assert_raises (Failure "Invalid addressing mode") (fun () ->
            Decode.address cpu mode)

(** Address tests to be run. *)
let address_tests : test list =
    [
        address_test "address absolute" cpu_zero (Absolute ~..0) ~..0;
        address_test "address absolute x" cpu_zero (AbsoluteX ~..0) ~..0;
        address_test "address absolute y" cpu_zero (AbsoluteY ~..0) ~..0;
        address_test "address indirect" cpu_zero (Indirect ~..0) ~..0;
        address_test "address x indirect" cpu_zero (XIndirect ~.0) ~..0;
        address_test "address indirect y" cpu_zero (IndirectY ~.0) ~..0;
        address_test "address relative" cpu_zero (Relative ~.0) ~..0;
        address_test "address zero page" cpu_zero (Zeropage ~.0) ~..0;
        address_test "address zero page x" cpu_zero (ZeropageX ~.0) ~..0;
        address_test "address zero page y" cpu_zero (ZeropageY ~.0) ~..0;
        address_raise_test "address accumulator" cpu_zero Accumulator;
        address_raise_test "address immediate" cpu_zero (Immediate ~.0);
    ]

(** [increment_pc_test name cpu size exp] asserts the equivalence between [increment_pc
    cpu size] and [exp]. *)
let increment_pc_test (name : string) (cpu : Cpu.t) (size : int) (exp : uint16)
    : test =
    name >:: fun _ ->
        assert_equal exp (Decode.increment_pc cpu size).progCounter
            ~printer:UInt16.to_string

(** Increment_pc tests to be run. *)
let increment_pc_tests : test list =
    [
        increment_pc_test "incr cpu pc 0" cpu_zero 0 ~..0;
        increment_pc_test "incr cpu pc 1" cpu_zero 1 ~..1;
        increment_pc_test "incr cpu pc 2" cpu_zero 2 ~..2;
        increment_pc_test "incr cpu pc 3" cpu_zero 3 ~..3;
    ]

(** [fetch_ui8_op_test name cpu exp] asserts the equivalence between [fetch_ui8_op
    cpu] and [exp]. *)
let fetch_ui8_op_test (name : string) (cpu : Cpu.t) (exp : uint8) : test =
    name >:: fun _ ->
        assert_equal exp (Decode.fetch_ui8_op cpu) ~printer:UInt8.to_string

(** Fetch_ui8_op tests to be run. *)
let fetch_ui8_op_tests : test list =
    [
        fetch_ui8_op_test "fetch ui8 op cpu_zero" cpu_zero ~.0;
        fetch_ui8_op_test "fetch ui8 op cpu_ui8" cpu_ui8 ~.128;
    ]

(** [fetch_ui16_op_test name cpu exp] asserts the equivalence between
    [fetch_ui16_op cpu] and [exp]. *)
let fetch_ui16_op_test (name : string) (cpu : Cpu.t) (exp : uint16) : test =
    name >:: fun _ ->
        assert_equal exp (Decode.fetch_ui16_op cpu) ~printer:UInt16.to_string

(** Fetch_ui16_op tests to be run. *)
let fetch_ui16_op_tests : test list =
    [
        fetch_ui16_op_test "fetch ui16 op cpu_zero" cpu_zero ~..0;
        fetch_ui16_op_test "fetch ui16 op cpu_ui8" cpu_ui8 ~..128;
    ]

(** Decode tests to be run. *)
let tests : test list =
    List.flatten
        [
            contents_tests;
            address_tests;
            increment_pc_tests;
            fetch_ui8_op_tests;
            fetch_ui16_op_tests;
        ]
