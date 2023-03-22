open OUnit2
open Lib__UInt8
open Lib__UInt16
open UIntXTest
module UInt16Test = F (UInt16)

let (to_string_tests : test list) =
    [
        UInt16Test.to_string_test "To_String Zero -> $0000" UInt16.zero "$0000";
        UInt16Test.to_string_test "To String Ones -> $0001" UInt16.one "$0001";
        UInt16Test.to_string_test "To_String Maxx -> $FFFF" UInt16.max_value "$FFFF";
        UInt16Test.to_string_test "To_String Over -> $0000"
            (UInt16.from_int UInt16Test.over)
            "$0000";
    ]

let ui16_from_ui8_test (name : string) (a : uint8) (expected : uint16) =
    name >:: fun _ ->
        assert_equal expected (UInt16.ui16_from_ui8 a) ~printer:UInt16.to_string

let (ui16_from_ui8_tests : test list) =
    [
        ui16_from_ui8_test "UI16_From_UI8 Zero -> $0000" UInt8.zero UInt16.zero;
        ui16_from_ui8_test "UI16_From_UI8 Ones -> $0001" UInt8.one UInt16.one;
        ui16_from_ui8_test "UI16_From_UI8 Maxx -> $00FF" UInt8.max_value
            (UInt16.from_int 0xFF);
    ]

let ui16_combine_ui8_test (name : string) (a : uint8) (b : uint8)
        (expected : uint16) =
    name >:: fun _ ->
        assert_equal expected (UInt16.ui16_combine_ui8 a b) ~printer:UInt16.to_string

let (ui16_combine_ui8_tests : test list) =
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

let (tests : test list) =
    UInt16Test.tests @ to_string_tests @ ui16_from_ui8_tests
    @ ui16_combine_ui8_tests
