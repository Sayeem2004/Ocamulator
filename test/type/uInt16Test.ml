open OUnit2;;
open Lib__UInt16;;
open UIntXTest;;

module UInt16Test = F(UInt16);;

let (to_string_tests: test list) = [
    UInt16Test.to_string_test "To_String Zero -> $0000" (UInt16.zero) ("$0000");
    UInt16Test.to_string_test "To String Ones -> $0001" (UInt16.one) ("$0001");
    UInt16Test.to_string_test "To_String Maxx -> $FFFF" (UInt16.max_value) ("$FFFF");
    UInt16Test.to_string_test "To_String Over -> $0000" (UInt16.from_int UInt16Test.over) ("$0000");
];;

let (tests: test list) = UInt16Test.tests @ to_string_tests;;
