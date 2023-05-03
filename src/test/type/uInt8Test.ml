open Lib
open OUnit2
open Lib.Alias

(** [UInt8Test.ml] contains coverage and accuracy tests for [UInt8.ml]. *)

module UInt8Tester = UIntXTest.Tester (UInt8)
(** UInt8Tester is a test module resulting from the UIntX functor. *)

(** String tests to be run. *)
let to_string_tests : test list =
    [
        UInt8Tester.to_string_test "To_String Zero -> $0000" UInt8.zero "$0000";
        UInt8Tester.to_string_test "To String Ones -> $0001" UInt8.one "$0001";
        UInt8Tester.to_string_test "To_String Maxx -> $00FF" UInt8.max "$00FF";
        UInt8Tester.to_string_test "To_String Over -> $0000" ~.UInt8Tester.over
            "$0000";
    ]

(** UInt8 tests to be run. *)
let tests : test list = List.flatten [ UInt8Tester.tests; to_string_tests ]
