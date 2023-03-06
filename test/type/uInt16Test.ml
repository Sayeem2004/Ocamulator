open OUnit2;;
open Lib__UInt16;;

let to_string_test (name: string) (input: uint16) (expected: string) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.to_string input) ~printer:(fun x -> x)
;;

let (to_string_tests: test list) = [
    to_string_test "To_String 0x00000 -> $0000" (UInt16.zero) ("$0000");
    to_string_test "To_String 0x00001 -> $0001" (UInt16.one) ("$0001");
    to_string_test "To_String 0x0FFFF -> $FFFF" (UInt16.max_value) ("$FFFF");
    to_string_test "To_String 0x10000 -> $0000" (UInt16.from_int 65536) ("$0000");
];;

let compare_test (name: string) (a: uint16) (b: uint16) (expected: int) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.compare a b) ~printer:string_of_int
;;

let (compare_tests: test list) = [
    compare_test "Compare 0x00000 0x00000 -> 0" (UInt16.zero) (UInt16.zero) (0);
    compare_test "Compare 0x00000 0x00001 -> -1" (UInt16.zero) (UInt16.one) (-1);
    compare_test "Compare 0x00001 0x00000 -> 1" (UInt16.one) (UInt16.zero) (1);
    compare_test "Compare 0x00000 0x0FFFF -> -1" (UInt16.zero) (UInt16.max_value) (-1);
    compare_test "Compare 0x0FFFF 0x00000 -> 1" (UInt16.max_value) (UInt16.one) (1);
    compare_test "Compare 0x00000 0x10000 -> 0" (UInt16.zero) (UInt16.from_int 65536) (0);
    compare_test "Compare 0x10000 0x00001 -> -1" (UInt16.from_int 65536) (UInt16.one) (-1);
];;

let equal_test (name: string) (a: uint16) (b: uint16) (expected: bool) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.equal a b) ~printer:string_of_bool
;;

let (equal_tests: test list) = [
    equal_test "Equal 0x00000 0x00000 -> true" (UInt16.zero) (UInt16.zero) (true);
    equal_test "Equal 0x00000 0x00001 -> false" (UInt16.zero) (UInt16.one) (false);
    equal_test "Equal 0x00001 0x00000 -> false" (UInt16.one) (UInt16.zero) (false);
    equal_test "Equal 0x00000 0x0FFFF -> false" (UInt16.zero) (UInt16.max_value) (false);
    equal_test "Equal 0x0FFFF 0x00000 -> false" (UInt16.max_value) (UInt16.one) (false);
    equal_test "Equal 0x00000 0x10000 -> true" (UInt16.zero) (UInt16.from_int 65536) (true);
    equal_test "Equal 0x10000 0x00001 -> false" (UInt16.from_int 65536) (UInt16.one) (false);
];;

let add_test (name: string) (a: uint16) (b: uint16) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.add a b) ~printer:UInt16.to_string
;;

let (add_tests: test list) = [
    add_test "Add 0x00000 0x00000 -> 0x00000" (UInt16.zero) (UInt16.zero) (UInt16.zero);
    add_test "Add 0x00000 0x00001 -> 0x00001" (UInt16.zero) (UInt16.one) (UInt16.one);
    add_test "Add 0x00001 0x00000 -> 0x00001" (UInt16.one) (UInt16.zero) (UInt16.one);
    add_test "Add 0x00000 0x0FFFF -> 0x0FFFF" (UInt16.zero) (UInt16.max_value) (UInt16.max_value);
    add_test "Add 0x0FFFF 0x00000 -> 0x0FFFF" (UInt16.max_value) (UInt16.zero) (UInt16.max_value);
    add_test "Add 0x00000 0x10000 -> 0x00000" (UInt16.zero) (UInt16.from_int 65536) (UInt16.zero);
    add_test "Add 0x10000 0x00001 -> 0x00001" (UInt16.from_int 65536) (UInt16.one) (UInt16.one);
];;

let sub_test (name: string) (a: uint16) (b: uint16) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.sub a b) ~printer:UInt16.to_string
;;

let (sub_tests: test list) = [
    sub_test "Sub 0x00000 0x00000 -> 0x00000" (UInt16.zero) (UInt16.zero) (UInt16.zero);
    sub_test "Sub 0x00000 0x00001 -> 0x000FF" (UInt16.zero) (UInt16.one) (UInt16.max_value);
    sub_test "Sub 0x00001 0x00000 -> 0x00001" (UInt16.one) (UInt16.zero) (UInt16.one);
    sub_test "Sub 0x00000 0x0FFFF -> 0x00001" (UInt16.zero) (UInt16.max_value) (UInt16.one);
    sub_test "Sub 0x0FFFF 0x00000 -> 0x0FFFF" (UInt16.max_value) (UInt16.zero) (UInt16.max_value);
    sub_test "Sub 0x00000 0x00100 -> 0x00000" (UInt16.zero) (UInt16.from_int 65536) (UInt16.zero);
    sub_test "Sub 0x10000 0x00001 -> 0x0FFFF" (UInt16.from_int 65536) (UInt16.one) (UInt16.max_value);
];;

let mul_test (name: string) (a: uint16) (b: uint16) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.mul a b) ~printer:UInt16.to_string
;;

let (mul_tests: test list) = [
    mul_test "Mul 0x00000 0x00000 -> 0x00000" (UInt16.zero) (UInt16.zero) (UInt16.zero);
    mul_test "Mul 0x00000 0x00001 -> 0x00000" (UInt16.zero) (UInt16.one) (UInt16.zero);
    mul_test "Mul 0x00001 0x00000 -> 0x00000" (UInt16.one) (UInt16.zero) (UInt16.zero);
    mul_test "Mul 0x00001 0x0FFFF -> 0x0FFFF" (UInt16.one) (UInt16.max_value) (UInt16.max_value);
    mul_test "Mul 0x0FFFF 0x00001 -> 0x0FFFF" (UInt16.max_value) (UInt16.one) (UInt16.max_value);
    mul_test "Mul 0x00001 0x10000 -> 0x00000" (UInt16.one) (UInt16.from_int 65536) (UInt16.zero);
    mul_test "Mul 0x10000 0x00001 -> 0x00000" (UInt16.from_int 65536) (UInt16.one) (UInt16.zero);
];;

let div_test (name: string) (a: uint16) (b: uint16) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.div a b) ~printer:UInt16.to_string
;;

let (div_tests: test list) = [
    div_test "Div 0x00000 0x00001 -> 0x00000" (UInt16.zero) (UInt16.one) (UInt16.zero);
    div_test "Div 0x00001 0x0FFFF -> 0x00000" (UInt16.one) (UInt16.max_value) (UInt16.zero);
    div_test "Div 0x0FFFF 0x00001 -> 0x000FF" (UInt16.max_value) (UInt16.one) (UInt16.max_value);
    div_test "Div 0x10000 0x00001 -> 0x10000" (UInt16.from_int 65536) (UInt16.one) (UInt16.from_int 65536);
];;

let rem_test (name: string) (a: uint16) (b: uint16) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.rem a b) ~printer:UInt16.to_string
;;

let (rem_tests: test list) = [
    rem_test "Rem 0x00000 0x00001 -> 0x00000" (UInt16.zero) (UInt16.one) (UInt16.zero);
    rem_test "Rem 0x00001 0x0FFFF -> 0x00001" (UInt16.one) (UInt16.max_value) (UInt16.one);
    rem_test "Rem 0x0FFFF 0x00001 -> 0x00000" (UInt16.max_value) (UInt16.one) (UInt16.zero);
    rem_test "Rem 0x10000 0x00001 -> 0x00000" (UInt16.from_int 65536) (UInt16.one) (UInt16.zero);
];;

let succ_test (name: string) (a: uint16) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.succ a) ~printer:UInt16.to_string
;;

let (succ_tests: test list) = [
    succ_test "Succ 0x00000 -> 0x00001" (UInt16.zero) (UInt16.one);
    succ_test "Succ 0x000FF -> 0x00000" (UInt16.max_value) (UInt16.zero);
];;

let pred_test (name: string) (a: uint16) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.pred a) ~printer:UInt16.to_string
;;

let (pred_tests: test list) = [
    pred_test "Pred 0x00001 -> 0x00000" (UInt16.one) (UInt16.zero);
    pred_test "Pred 0x00000 -> 0x0FFFF" (UInt16.zero) (UInt16.max_value);
];;

let logand_test (name: string) (a: uint16) (b: uint16) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.logand a b) ~printer:UInt16.to_string
;;

let (logand_tests: test list) = [
    logand_test "Logand 0x00000 0x00000 -> 0x00000" (UInt16.zero) (UInt16.zero) (UInt16.zero);
    logand_test "Logand 0x00000 0x00001 -> 0x00000" (UInt16.zero) (UInt16.one) (UInt16.zero);
    logand_test "Logand 0x00001 0x00000 -> 0x00000" (UInt16.one) (UInt16.zero) (UInt16.zero);
    logand_test "Logand 0x00001 0x0FFFF -> 0x00001" (UInt16.one) (UInt16.max_value) (UInt16.one);
    logand_test "Logand 0x0FFFF 0x0FFFF -> 0x0FFFF" (UInt16.max_value) (UInt16.max_value) (UInt16.max_value);
    logand_test "Logand 0x00001 0x10000 -> 0x00000" (UInt16.one) (UInt16.from_int 65536) (UInt16.zero);
    logand_test "Logand 0x10000 0x00001 -> 0x00000" (UInt16.from_int 65536) (UInt16.one) (UInt16.zero);
];;

let logor_test (name: string) (a: uint16) (b: uint16) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.logor a b) ~printer:UInt16.to_string
;;

let (logor_tests: test list) = [
    logor_test "Logor 0x00000 0x00000 -> 0x00000" (UInt16.zero) (UInt16.zero) (UInt16.zero);
    logor_test "Logor 0x00000 0x00001 -> 0x00001" (UInt16.zero) (UInt16.one) (UInt16.one);
    logor_test "Logor 0x00001 0x00000 -> 0x00001" (UInt16.one) (UInt16.zero) (UInt16.one);
    logor_test "Logor 0x00001 0x0FFFF -> 0x0FFFF" (UInt16.one) (UInt16.max_value) (UInt16.max_value);
    logor_test "Logor 0x0FFFF 0x0FFFF -> 0x0FFFF" (UInt16.max_value) (UInt16.max_value) (UInt16.max_value);
    logor_test "Logor 0x00001 0x10000 -> 0x00001" (UInt16.one) (UInt16.from_int 65536) (UInt16.one);
    logor_test "Logor 0x10000 0x00001 -> 0x00001" (UInt16.from_int 65536) (UInt16.one) (UInt16.one);
];;

let logxor_test (name: string) (a: uint16) (b: uint16) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.logxor a b) ~printer:UInt16.to_string
;;

let (logxor_tests: test list) = [
    logxor_test "Logxor 0x00000 0x00000 -> 0x00000" (UInt16.zero) (UInt16.zero) (UInt16.zero);
    logxor_test "Logxor 0x00000 0x00001 -> 0x00001" (UInt16.zero) (UInt16.one) (UInt16.one);
    logxor_test "Logxor 0x00001 0x00000 -> 0x00001" (UInt16.one) (UInt16.zero) (UInt16.one);
    logxor_test "Logxor 0x00001 0x0FFFF -> 0x0FFFE" (UInt16.one) (UInt16.max_value) (UInt16.from_int 65534);
    logxor_test "Logxor 0x0FFFF 0x0FFFF -> 0x00000" (UInt16.max_value) (UInt16.max_value) (UInt16.zero);
    logxor_test "Logxor 0x00001 0x10000 -> 0x00001" (UInt16.one) (UInt16.from_int 65536) (UInt16.one);
    logxor_test "Logxor 0x10000 0x00001 -> 0x00001" (UInt16.from_int 65536) (UInt16.one) (UInt16.one);
];;

let shift_left_test (name: string) (a: uint16) (b: int) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.shift_left a b) ~printer:UInt16.to_string
;;

let (shift_left_tests: test list) = [
    shift_left_test "Shift_left 0x00000 0 -> 0x00000" (UInt16.zero) (0) (UInt16.zero);
    shift_left_test "Shift_left 0x00000 1 -> 0x00000" (UInt16.zero) (1) (UInt16.zero);
    shift_left_test "Shift_left 0x00001 0 -> 0x00001" (UInt16.one) (0) (UInt16.one);
    shift_left_test "Shift_left 0x00001 1 -> 0x00002" (UInt16.one) (1) (UInt16.from_int 2);
    shift_left_test "Shift_left 0x00001 15 -> 0x08000" (UInt16.one) (15) (UInt16.from_int 32768);
    shift_left_test "Shift_left 0x00001 16 -> 0x00000" (UInt16.one) (16) (UInt16.zero);
    shift_left_test "Shift_left 0x0FFFF 1 -> 0x0FFFE" (UInt16.from_int 65535) (1) (UInt16.from_int 65534);
];;

let shift_right_test (name: string) (a: uint16) (b: int) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.shift_right a b) ~printer:UInt16.to_string
;;

let (shift_right_tests: test list) = [
    shift_right_test "Shift_right 0x00000 0 -> 0x00000" (UInt16.zero) (0) (UInt16.zero);
    shift_right_test "Shift_right 0x00000 1 -> 0x00000" (UInt16.zero) (1) (UInt16.zero);
    shift_right_test "Shift_right 0x00001 0 -> 0x00001" (UInt16.one) (0) (UInt16.one);
    shift_right_test "Shift_right 0x00001 1 -> 0x00000" (UInt16.one) (1) (UInt16.zero);
    shift_right_test "Shift_right 0x00001 7 -> 0x00000" (UInt16.one) (7) (UInt16.zero);
    shift_right_test "Shift_right 0x00001 8 -> 0x00000" (UInt16.one) (8) (UInt16.zero);
    shift_right_test "Shift_right 0x0FFFF 1 -> 0x07FFF" (UInt16.max_value) (1) (UInt16.from_int 32767);
    shift_right_test "Shift_right 0x0FFFF 15 -> 0x00001" (UInt16.max_value) (15) (UInt16.one);
];;

let from_int_test (name: string) (a: int) (expected: uint16) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.from_int a) ~printer:UInt16.to_string
;;

let (from_int_tests: test list) = [
    from_int_test "From_int 00000 -> 0x00000" (0) (UInt16.zero);
    from_int_test "From_int 00001 -> 0x00001" (1) (UInt16.one);
    from_int_test "From_int 65535 -> 0x0FFFF" (65535) (UInt16.max_value);
    from_int_test "From_int 65536 -> 0x00000" (65536) (UInt16.zero);
];;

let to_int_test (name: string) (a: uint16) (expected: int) : test =
    name >:: fun _ ->
        assert_equal expected (UInt16.to_int a) ~printer:string_of_int
;;

let (to_int_tests: test list) = [
    to_int_test "To_int 0x00000 -> 00000" (UInt16.zero) (0);
    to_int_test "To_int 0x00001 -> 00001" (UInt16.one) (1);
    to_int_test "To_int 0x0FFFF -> 65535" (UInt16.max_value) (65535);
];;

let (tests: test list) = List.flatten [
    to_string_tests; compare_tests; equal_tests; add_tests; sub_tests; mul_tests;
    div_tests; rem_tests; succ_tests; pred_tests; logand_tests; logor_tests;
    logxor_tests; shift_left_tests; shift_right_tests; from_int_tests; to_int_tests;
];;
