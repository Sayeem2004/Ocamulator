open OUnit2
open Lib__UInt8
open UIntXTest

module UInt8Test = F (UInt8)
(** UInt8Test is a test module resulting from the UIntX functor. *)

(** String tests to be run. *)
let (to_string_tests : test list) =
    [
        UInt8Test.to_string_test "To_String Zero -> $0000" UInt8.zero "$0000";
        UInt8Test.to_string_test "To String Ones -> $0001" UInt8.one "$0001";
        UInt8Test.to_string_test "To_String Maxx -> $00FF" UInt8.max_value "$00FF";
        UInt8Test.to_string_test "To_String Over -> $0000"
            (UInt8.from_int UInt8Test.over)
            "$0000";
    ]

(** UInt8 tests to be run. *)
let (tests : test list) = UInt8Test.tests @ to_string_tests
