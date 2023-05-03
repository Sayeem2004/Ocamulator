open OUnit2
open Lib__UInt8
open Lib__UInt16
open Lib__Ram

(** [ram_zero] is an empty RAM array. *)
let ram_zero : RAM.t = RAM.nes_ram (Bytes.make 0xFFFF '\x00')

(** [ram_ui8] is a RAM array with value 128 set at position 0. *)
let ram_ui8 : RAM.t =
    let ram : RAM.t = RAM.zero_ram () in
    RAM.write_ui8 ram (UInt16.from_int 0) (UInt8.from_int 128);
    ram

(** [read_ui8_test name ram addr exp] tests equivalence between [read_ui8 ram
    addr] and [exp]. *)
let read_ui8_test (name : string) (ram : RAM.t) (addr : UInt16.t)
        (exp : UInt8.t) : test =
    name >:: fun _ ->
        assert_equal exp (RAM.read_ui8 ram addr) ~printer:UInt8.to_string

(** Read_ui8 tests to be run. *)
let read_ui8_tests : test list =
    [
        read_ui8_test "read_ui8_test 0" ram_zero (UInt16.from_int 0) UInt8.zero;
        read_ui8_test "read_ui8_test 1" ram_ui8 (UInt16.from_int 0)
            (UInt8.from_int 128);
    ]

(** [write_ui8_test name ram addr exp] tests equivalence between [write_ui8 ram
    addr exp] and [exp]. *)
let write_ui8_test (name : string) (ram : RAM.t) (addr : UInt16.t)
        (exp : UInt8.t) : test =
    name >:: fun _ ->
        let _ = RAM.write_ui8 ram addr exp in
        assert_equal exp (RAM.read_ui8 ram addr) ~printer:UInt8.to_string

(** Write_ui8 tests to be run. *)
let write_ui8_tests : test list =
    [
        write_ui8_test "write_ui8_test 0" ram_zero (UInt16.from_int 0) UInt8.zero;
        write_ui8_test "write_ui8_test 1" ram_zero (UInt16.from_int 0)
            (UInt8.from_int 128);
    ]

(** Ram tests to be run. *)
let tests : test list = List.flatten [ read_ui8_tests; write_ui8_tests ]
