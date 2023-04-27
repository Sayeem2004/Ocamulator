open OUnit2
open Lib__UInt8
open Lib__UInt16
open UIntXTest

module UInt16Test = F (UInt16)
(** UInt16Test is a test module resulting from the UIntX functor. *)

(** String_tests to be run. *)
let to_string_tests : test list =
    [
        UInt16Test.to_string_test "To_String Zero -> $0000" UInt16.zero "$0000";
        UInt16Test.to_string_test "To String Ones -> $0001" UInt16.one "$0001";
        UInt16Test.to_string_test "To_String Maxx -> $FFFF" UInt16.max_value "$FFFF";
        UInt16Test.to_string_test "To_String Over -> $0000"
            (UInt16.from_int UInt16Test.over)
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
        ui16_from_ui8_test "UI16_From_UI8 Maxx -> $00FF" UInt8.max_value
            (UInt16.from_int 0xFF);
    ]

(** [ui16_combine_ui8_test name a b expected] tests equivalence between
    [ui16_combine_ui8 a b] and [expected]. *)
let ui16_combine_ui8_test (name : string) (a : uint8) (b : uint8)
        (expected : uint16) =
    name >:: fun _ ->
        assert_equal expected (UInt16.ui16_combine_ui8 a b) ~printer:UInt16.to_string

(** Ui16_combine_ui8 tests to be run. *)
let ui16_combine_ui8_tests : test list =
    [
        ui16_combine_ui8_test "UI16_Combine_UI8 Zero Zero -> $0000" UInt8.zero
            UInt8.zero UInt16.zero;
        ui16_combine_ui8_test "UI16_Combine_UI8 Zero Ones -> $0001" UInt8.zero
            UInt8.one UInt16.one;
        ui16_combine_ui8_test "UI16_Combine_UI8 Ones Zero -> $0100" UInt8.one
            UInt8.zero (UInt16.from_int 0x0100);
        ui16_combine_ui8_test "UI16_Combine_UI8 Ones Ones -> $0101" UInt8.one
            UInt8.one (UInt16.from_int 0x0101);
        ui16_combine_ui8_test "UI16_Combine_UI8 Maxx Maxx -> $FFFF" UInt8.max_value
            UInt8.max_value UInt16.max_value;
    ]

(** [bool_to_ui16 name b expected] tests equivalence between [?^ b] and [expected]. *)
let bool_to_ui16_test (name : string) (b : bool) (expected : uint16) =
    name >:: fun _ -> assert_equal expected ?^b ~printer:UInt16.to_string

(** Bool_to_ui16 tests to be run. *)
let bool_to_ui16_tests : test list =
    [
        bool_to_ui16_test "Bool_to_UI16 False -> $0000" false UInt16.zero;
        bool_to_ui16_test "Bool_to_UI16 True -> $0001" true UInt16.one;
    ]

(** [ui16_to_ui8 name n expected] tests equivalence between [!. n] and [expected]. *)
let ui16_to_ui8_test (name : string) (n : uint16) (expected : uint8) =
    name >:: fun _ -> assert_equal expected !.n ~printer:UInt8.to_string

(** Ui16_to_ui8 tests to be run. *)
let ui16_to_ui8_tests : test list =
    [
        ui16_to_ui8_test "UI16_to_UI8 Zero -> $00" UInt16.zero UInt8.zero;
        ui16_to_ui8_test "UI16_to_UI8 Ones -> $01" UInt16.one UInt8.one;
        ui16_to_ui8_test "UI16_to_UI8 Maxx -> $FF" UInt16.max_value UInt8.max_value;
    ]

(** UInt16 tests to be run. *)
let tests : test list = List.flatten [
    UInt16Test.tests;
    to_string_tests;
    ui16_from_ui8_tests;
    ui16_combine_ui8_tests;
    bool_to_ui16_tests;
    ui16_to_ui8_tests;
]
