open Ocamulator
open OUnit2
open Ocamulator.Alias

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

(** [ui8_from_ui4_test name a exp] tests equivalence between [ui8_from_ui4 a]
    and [exp]. *)
let ui8_from_ui4_test (name : string) (a : uint4) (exp : uint8) : test =
    name >:: fun _ ->
        assert_equal exp (UInt8.ui8_from_ui4 a) ~printer:UInt8.to_string

(** Ui8_from_ui4 tests to be run. *)
let ui8_from_ui4_tests : test list =
    [
        ui8_from_ui4_test "Ui8_From_Ui4 Zero" ~...0x00 ~.0x00;
        ui8_from_ui4_test "Ui8_From_Ui4 Ones" ~...0x01 ~.0x01;
        ui8_from_ui4_test "Ui8_From_Ui4 Maxx" ~...0x0F ~.0x0F;
    ]

(** [combine_ui4_test name a b exp] tests equivalence between [combine_ui4 a b]
    and [exp]. *)
let combine_ui4_test (name : string) (a : uint4) (b : uint4) (exp : uint8) =
    name >:: fun _ ->
        assert_equal exp (UInt8.combine_ui4 a b) ~printer:UInt8.to_string

(** Combine_ui4 tests to be run. *)
let combine_ui4_tests : test list =
    [
        combine_ui4_test "Combine_Ui4 Zero Zero -> $0000" ~...0x00 ~...0x00 ~.0x00;
        combine_ui4_test "Combine_Ui4 Zero Ones -> $0001" ~...0x00 ~...0x01 ~.0x01;
        combine_ui4_test "Combine_Ui4 Ones Zero -> $0010" ~...0x01 ~...0x00 ~.0x10;
        combine_ui4_test "Combine_Ui4 Ones Ones -> $0011" ~...0x01 ~...0x01 ~.0x11;
        combine_ui4_test "Combine_Ui4 Maxx Maxx -> $00FF" ~...0x0F ~...0x0F ~.0xFF;
    ]

(** UInt8 tests to be run. *)
let tests : test list =
    List.flatten
        [
            UInt8Tester.tests; to_string_tests; ui8_from_ui4_tests; combine_ui4_tests;
        ]
