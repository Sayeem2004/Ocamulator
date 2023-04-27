open OUnit2
open Lib__UInt8
open Lib__UInt16
open Lib__Ram

(** [ram_zero] is an empty RAM array. *)
let ram_zero : RAM.t = RAM.nes_ram (Bytes.make 0xFFFF '\x00')

(** [ram_ui8] is a RAM array with value 128 set at position 0. *)
let ram_ui8 : RAM.t =
    let ram : RAM.t = RAM.nes_zero_ram () in
    RAM.write_ui8 ram (UInt16.from_int 0) (UInt8.from_int 128); ram

(** [ram_ui16] is a RAM array with value 256 set at position 0. *)
let ram_ui16 : RAM.t =
    let ram : RAM.t = RAM.nes_zero_ram () in
    RAM.write_ui16 ram (UInt16.from_int 0) (UInt16.from_int 256); ram

(** [byte_to_uint8_test name c exp] tests equivalence between [byte_to_uint8 c]
    and [exp]. *)
let byte_to_uint8_test (name : string) (c : char) (exp: uint8) : test =
    name >:: fun _ ->
        assert_equal exp (RAM.byte_to_uint8 c) ~printer:UInt8.to_string

(** Byte_to_uint8 tests to be run. *)
let byte_to_uint8_tests : test list = [
    byte_to_uint8_test "byte_to_uint8_test 0" '\x00' UInt8.zero;
    byte_to_uint8_test "byte_to_uint8_test 1" '\x01' UInt8.one;
    byte_to_uint8_test "byte_to_uint8_test 128" '\x80' (UInt8.from_int 128);
    byte_to_uint8_test "byte_to_uint8_test 255" '\xff' (UInt8.from_int 255);
]

(** [uint8_to_byte_test name n exp] tests equivalence between [uint8_to_byte n]
    and [exp]. *)
let uint8_to_byte_test (name : string) (n : uint8) (exp: char) : test =
    name >:: fun _ ->
        assert_equal exp (RAM.uint8_to_byte n) ~printer:(String.make 1)

(** Uint8_to_byte tests to be run. *)
let uint8_to_byte_tests : test list = [
    uint8_to_byte_test "uint8_to_byte_test 0" UInt8.zero '\x00';
    uint8_to_byte_test "uint8_to_byte_test 1" UInt8.one '\x01';
    uint8_to_byte_test "uint8_to_byte_test 128" (UInt8.from_int 128) '\x80';
    uint8_to_byte_test "uint8_to_byte_test 255" (UInt8.from_int 255) '\xff';
]

(** [read_ui8_test name ram addr exp] tests equivalence between [read_ui8 ram
    addr] and [exp]. *)
let read_ui8_test (name : string) (ram : RAM.t) (addr : UInt16.t) (exp : UInt8.t) : test =
    name >:: fun _ ->
        assert_equal exp (RAM.read_ui8 ram addr) ~printer:UInt8.to_string

(** Read_ui8 tests to be run. *)
let read_ui8_tests : test list = [
    read_ui8_test "read_ui8_test 0" ram_zero (UInt16.from_int 0) UInt8.zero;
    read_ui8_test "read_ui8_test 1" ram_ui8 (UInt16.from_int 0) (UInt8.from_int 128);
    read_ui8_test "read_ui8_test 2" ram_ui16 (UInt16.from_int 0) (UInt8.zero);
]

(** [read_ui16_test name ram addr exp] tests equivalence between [read_ui16 ram
    add] and [exp]. *)
let read_ui16_test (name : string) (ram : RAM.t) (addr : UInt16.t) (exp : UInt16.t) : test =
    name >:: fun _ ->
        assert_equal exp (RAM.read_ui16 ram addr) ~printer:UInt16.to_string

(** Read_ui16 tests to be run. *)
let read_ui16_tests : test list = [
    read_ui16_test "read_ui16_test 0" ram_zero (UInt16.from_int 0) UInt16.zero;
    (* ! Could be wrong *)
    read_ui16_test "read_ui16_test 1" ram_ui8 (UInt16.from_int 0) (UInt16.from_int 32768);
    (* ! Could be wrong *)
    read_ui16_test "read_ui16_test 2" ram_ui16 (UInt16.from_int 0) (UInt16.from_int 1);
]

(** Ram tests to be run. *)
let tests : test list = List.flatten [
    byte_to_uint8_tests;
    uint8_to_byte_tests;
    read_ui8_tests;
    read_ui16_tests;
]
