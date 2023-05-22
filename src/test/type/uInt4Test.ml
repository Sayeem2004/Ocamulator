open Ocamulator
open OUnit2
open Ocamulator.Alias

(** [UInt4Test.ml] contains coverage and accuracy tests for [UInt4.ml]. *)

module UInt4Tester = UIntXTest.Tester (UInt4)
(** UInt4Tester is a test module resulting from the UIntX functor. *)

(** String tests to be run. *)
let to_string_tests : test list =
    [
        UInt4Tester.to_string_test "To_String Zero -> $0000" UInt4.zero "$0000";
        UInt4Tester.to_string_test "To String Ones -> $0001" UInt4.one "$0001";
        UInt4Tester.to_string_test "To_String Maxx -> $000F" UInt4.max "$000F";
        UInt4Tester.to_string_test "To_String Over -> $0000" ~...UInt4Tester.over
            "$0000";
    ]

(** UInt4 tests to be run. *)
let tests : test list = List.flatten [ UInt4Tester.tests; to_string_tests ]
