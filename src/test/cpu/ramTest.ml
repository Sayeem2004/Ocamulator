open Ocamulator
open OUnit2
open Ocamulator.Alias

(** [RamTest.ml] contains coverage and accuracy tests for [Ram.ml]. *)

(** [ram_zero] is an empty Ram array. *)
let ram_zero : Ram.t = Ram.nes_ram (Bytes.make 0xFFFF '\x00')

(** [ram_ui8] is a Ram array with value 128 set at position 0. *)
let ram_ui8 : Ram.t =
    let ram : Ram.t = Ram.zero_ram () in
    Ram.write_ui8 ram ~..0 ~.128;
    ram

(** [read_ui8_test name ram addr exp] tests equivalence between [read_ui8 ram
    addr] and [exp]. *)
let read_ui8_test (name : string) (ram : Ram.t) (addr : uint16) (exp : uint8) :
    test =
    name >:: fun _ ->
        assert_equal exp (Ram.read_ui8 ram addr) ~printer:UInt8.to_string

(** Read_ui8 tests to be run. *)
let read_ui8_tests : test list =
    [
        read_ui8_test "read_ui8_test 0" ram_zero ~..0 UInt8.zero;
        read_ui8_test "read_ui8_test 1" ram_ui8 ~..0 ~.128;
    ]

(** [write_ui8_test name ram addr exp] tests equivalence between [write_ui8 ram
    addr exp] and [exp]. *)
let write_ui8_test (name : string) (ram : Ram.t) (addr : uint16) (exp : uint8) :
    test =
    name >:: fun _ ->
        let _ = Ram.write_ui8 ram addr exp in
        assert_equal exp (Ram.read_ui8 ram addr) ~printer:UInt8.to_string

(** Write_ui8 tests to be run. *)
let write_ui8_tests : test list =
    [
        write_ui8_test "write_ui8_test 0" ram_zero ~..0 UInt8.zero;
        write_ui8_test "write_ui8_test 1" ram_zero ~..0 ~.128;
    ]

(** Ram tests to be run. *)
let tests : test list = List.flatten [ read_ui8_tests; write_ui8_tests ]
