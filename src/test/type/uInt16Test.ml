open Lib
open OUnit2
open Lib.Alias

(** [UInt16Test.ml] contains coverage and accuracy tests for [UInt16.ml]. *)

module UInt16Tester = UIntXTest.Tester (UInt16)
(** UInt16Tester is a test module resulting from the UIntXTest functor. *)

(** String_tests to be run. *)
let to_string_tests : test list =
    [
        UInt16Tester.to_string_test "To_String Zero -> $0000" UInt16.zero "$0000";
        UInt16Tester.to_string_test "To String Ones -> $0001" UInt16.one "$0001";
        UInt16Tester.to_string_test "To_String Maxx -> $FFFF" UInt16.max "$FFFF";
        UInt16Tester.to_string_test "To_String Over -> $0000" ~..UInt16Tester.over
            "$0000";
    ]

(** [ui16_from_ui8_test name a expected] tests equivalence between
    [ui16_from_ui8 a] and [expected]. *)
let ui16_from_ui8_test (name : string) (a : uint8) (expected : uint16) =
    name >:: fun _ ->
        assert_equal expected (UInt16.ui16_from_ui8 a) ~printer:UInt16.to_string

(** Ui16_from_ui8 tests to be run. *)
let ui16_from_ui8_tests : test list =
    [
        ui16_from_ui8_test "UI16_From_UI8 Zero -> $0000" UInt8.zero UInt16.zero;
        ui16_from_ui8_test "UI16_From_UI8 Ones -> $0001" UInt8.one UInt16.one;
        ui16_from_ui8_test "UI16_From_UI8 Maxx -> $00FF" UInt8.max ~..0xFF;
    ]

(** [combine_ui8_test name a b expected] tests equivalence between
    [combine_ui8 a b] and [expected]. *)
let combine_ui8_test (name : string) (a : uint8) (b : uint8) (expected : uint16)
    =
    name >:: fun _ ->
        assert_equal expected (UInt16.combine_ui8 a b) ~printer:UInt16.to_string

(** Combine_ui8 tests to be run. *)
let combine_ui8_tests : test list =
    [
        combine_ui8_test "Combine_UI8 Zero Zero -> $0000" UInt8.zero UInt8.zero
            UInt16.zero;
        combine_ui8_test "Combine_UI8 Zero Ones -> $0001" UInt8.zero UInt8.one
            UInt16.one;
        combine_ui8_test "Combine_UI8 Ones Zero -> $0100" UInt8.one UInt8.zero
            ~..0x0100;
        combine_ui8_test "Combine_UI8 Ones Ones -> $0101" UInt8.one UInt8.one
            ~..0x0101;
        combine_ui8_test "Combine_UI8 Maxx Maxx -> $FFFF" UInt8.max UInt8.max
            UInt16.max;
    ]

(** [split_ui16_test name n expected] tests equivalence between
    [split_ui16 n] and [expected]. *)
let split_ui16_test (name : string) (n : uint16) (expected : uint8 * uint8) =
    name >:: fun _ ->
        assert_equal expected (UInt16.split_ui16 n) ~printer:(fun (a, b) ->
            "(" ^ UInt8.to_string a ^ ", " ^ UInt8.to_string b ^ ")")

(** Split_ui16 tests to be run. *)
let split_ui16_tests : test list =
    [
        split_ui16_test "Split_UI16 Zero -> (Zero, Zero)" UInt16.zero
            (UInt8.zero, UInt8.zero);
        split_ui16_test "Split_UI16 Ones -> (Zero, Ones)" UInt16.one
            (UInt8.zero, UInt8.one);
        split_ui16_test "Split_UI16 Maxx -> (Maxx, Maxx)" UInt16.max
            (UInt8.max, UInt8.max);
    ]

(** UInt16 tests to be run. *)
let tests : test list =
    List.flatten
        [
            UInt16Tester.tests;
            to_string_tests;
            ui16_from_ui8_tests;
            combine_ui8_tests;
            split_ui16_tests;
        ]
