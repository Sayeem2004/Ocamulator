open OUnit2;;
open Lib__UInt8;;

let to_string_test (name: string) (input: uint8) (expected: string) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.to_string input) ~printer:(fun x -> x)
;;

let (to_string_tests: test list) = [
    to_string_test "To_String 0x0000 -> $0000" (UInt8.zero) ("$0000");
    to_string_test "To_String 0x0001 -> $0001" (UInt8.one) ("$0001");
    to_string_test "To_String 0x00FF -> $00FF" (UInt8.max_value) ("$00FF");
    to_string_test "To_String 0x0100 -> $0000" (UInt8.from_int 256) ("$0000");
];;

let compare_test (name: string) (a: uint8) (b: uint8) (expected: int) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.compare a b) ~printer:string_of_int
;;

let (compare_tests: test list) = [
    compare_test "Compare 0x0000 0x0000 -> 0" (UInt8.zero) (UInt8.zero) (0);
    compare_test "Compare 0x0000 0x0001 -> -1" (UInt8.zero) (UInt8.one) (-1);
    compare_test "Compare 0x0001 0x0000 -> 1" (UInt8.one) (UInt8.zero) (1);
    compare_test "Compare 0x0000 0x00FF -> -1" (UInt8.zero) (UInt8.max_value) (-1);
    compare_test "Compare 0x00FF 0x0000 -> 1" (UInt8.max_value) (UInt8.one) (1);
    compare_test "Compare 0x0000 0x0100 -> 0" (UInt8.zero) (UInt8.from_int 256) (0);
    compare_test "Compare 0x0100 0x0001 -> -1" (UInt8.from_int 256) (UInt8.one) (-1);
];;

let equal_test (name: string) (a: uint8) (b: uint8) (expected: bool) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.equal a b) ~printer:string_of_bool
;;

let (equal_tests: test list) = [
    equal_test "Equal 0x0000 0x0000 -> true" (UInt8.zero) (UInt8.zero) (true);
    equal_test "Equal 0x0000 0x0001 -> false" (UInt8.zero) (UInt8.one) (false);
    equal_test "Equal 0x0001 0x0000 -> false" (UInt8.one) (UInt8.zero) (false);
    equal_test "Equal 0x0000 0x00FF -> false" (UInt8.zero) (UInt8.max_value) (false);
    equal_test "Equal 0x00FF 0x0000 -> false" (UInt8.max_value) (UInt8.one) (false);
    equal_test "Equal 0x0000 0x0100 -> true" (UInt8.zero) (UInt8.from_int 256) (true);
    equal_test "Equal 0x0100 0x0001 -> false" (UInt8.from_int 256) (UInt8.one) (false);
];;

let add_test (name: string) (a: uint8) (b: uint8) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.add a b) ~printer:UInt8.to_string
;;

let (add_tests: test list) = [
    add_test "Add 0x0000 0x0000 -> 0x0000" (UInt8.zero) (UInt8.zero) (UInt8.zero);
    add_test "Add 0x0000 0x0001 -> 0x0001" (UInt8.zero) (UInt8.one) (UInt8.one);
    add_test "Add 0x0001 0x0000 -> 0x0001" (UInt8.one) (UInt8.zero) (UInt8.one);
    add_test "Add 0x0000 0x00FF -> 0x00FF" (UInt8.zero) (UInt8.max_value) (UInt8.max_value);
    add_test "Add 0x00FF 0x0000 -> 0x00FF" (UInt8.max_value) (UInt8.zero) (UInt8.max_value);
    add_test "Add 0x0000 0x0100 -> 0x0000" (UInt8.zero) (UInt8.from_int 256) (UInt8.zero);
    add_test "Add 0x0100 0x0001 -> 0x0001" (UInt8.from_int 256) (UInt8.one) (UInt8.one);
];;

let sub_test (name: string) (a: uint8) (b: uint8) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.sub a b) ~printer:UInt8.to_string
;;

let (sub_tests: test list) = [
    sub_test "Sub 0x0000 0x0000 -> 0x0000" (UInt8.zero) (UInt8.zero) (UInt8.zero);
    sub_test "Sub 0x0000 0x0001 -> 0x00FF" (UInt8.zero) (UInt8.one) (UInt8.max_value);
    sub_test "Sub 0x0001 0x0000 -> 0x0001" (UInt8.one) (UInt8.zero) (UInt8.one);
    sub_test "Sub 0x0000 0x00FF -> 0x0001" (UInt8.zero) (UInt8.max_value) (UInt8.one);
    sub_test "Sub 0x00FF 0x0000 -> 0x00FF" (UInt8.max_value) (UInt8.zero) (UInt8.max_value);
    sub_test "Sub 0x0000 0x0100 -> 0x0000" (UInt8.zero) (UInt8.from_int 256) (UInt8.zero);
    sub_test "Sub 0x0100 0x0001 -> 0x00FF" (UInt8.from_int 256) (UInt8.one) (UInt8.max_value);
];;

let mul_test (name: string) (a: uint8) (b: uint8) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.mul a b) ~printer:UInt8.to_string
;;

let (mul_tests: test list) = [
    mul_test "Mul 0x0000 0x0000 -> 0x0000" (UInt8.zero) (UInt8.zero) (UInt8.zero);
    mul_test "Mul 0x0000 0x0001 -> 0x0000" (UInt8.zero) (UInt8.one) (UInt8.zero);
    mul_test "Mul 0x0001 0x0000 -> 0x0000" (UInt8.one) (UInt8.zero) (UInt8.zero);
    mul_test "Mul 0x0001 0x00FF -> 0x00FF" (UInt8.one) (UInt8.max_value) (UInt8.max_value);
    mul_test "Mul 0x00FF 0x0001 -> 0x00FF" (UInt8.max_value) (UInt8.one) (UInt8.max_value);
    mul_test "Mul 0x0001 0x0100 -> 0x0000" (UInt8.one) (UInt8.from_int 256) (UInt8.zero);
    mul_test "Mul 0x0100 0x0001 -> 0x0000" (UInt8.from_int 256) (UInt8.one) (UInt8.zero);
];;

let div_test (name: string) (a: uint8) (b: uint8) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.div a b) ~printer:UInt8.to_string
;;

let (div_tests: test list) = [
    div_test "Div 0x0000 0x0001 -> 0x0000" (UInt8.zero) (UInt8.one) (UInt8.zero);
    div_test "Div 0x0001 0x00FF -> 0x0000" (UInt8.one) (UInt8.max_value) (UInt8.zero);
    div_test "Div 0x00FF 0x0001 -> 0x00FF" (UInt8.max_value) (UInt8.one) (UInt8.max_value);
    div_test "Div 0x0100 0x0001 -> 0x0100" (UInt8.from_int 256) (UInt8.one) (UInt8.from_int 256);
];;

let rem_test (name: string) (a: uint8) (b: uint8) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.rem a b) ~printer:UInt8.to_string
;;

let (rem_tests: test list) = [
    rem_test "Rem 0x0000 0x0001 -> 0x0000" (UInt8.zero) (UInt8.one) (UInt8.zero);
    rem_test "Rem 0x0001 0x00FF -> 0x0001" (UInt8.one) (UInt8.max_value) (UInt8.one);
    rem_test "Rem 0x00FF 0x0001 -> 0x0000" (UInt8.max_value) (UInt8.one) (UInt8.zero);
    rem_test "Rem 0x0100 0x0001 -> 0x0000" (UInt8.from_int 256) (UInt8.one) (UInt8.zero);
];;

let succ_test (name: string) (a: uint8) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.succ a) ~printer:UInt8.to_string
;;

let (succ_tests: test list) = [
    succ_test "Succ 0x0000 -> 0x0001" (UInt8.zero) (UInt8.one);
    succ_test "Succ 0x00FF -> 0x0000" (UInt8.max_value) (UInt8.zero);
];;

let pred_test (name: string) (a: uint8) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.pred a) ~printer:UInt8.to_string
;;

let (pred_tests: test list) = [
    pred_test "Pred 0x0001 -> 0x0000" (UInt8.one) (UInt8.zero);
    pred_test "Pred 0x0000 -> 0x00FF" (UInt8.zero) (UInt8.max_value);
];;

let logand_test (name: string) (a: uint8) (b: uint8) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.logand a b) ~printer:UInt8.to_string
;;

let (logand_tests: test list) = [
    logand_test "Logand 0x0000 0x0000 -> 0x0000" (UInt8.zero) (UInt8.zero) (UInt8.zero);
    logand_test "Logand 0x0000 0x0001 -> 0x0000" (UInt8.zero) (UInt8.one) (UInt8.zero);
    logand_test "Logand 0x0001 0x0000 -> 0x0000" (UInt8.one) (UInt8.zero) (UInt8.zero);
    logand_test "Logand 0x0001 0x00FF -> 0x0001" (UInt8.one) (UInt8.max_value) (UInt8.one);
    logand_test "Logand 0x00FF 0x00FF -> 0x00FF" (UInt8.max_value) (UInt8.max_value) (UInt8.max_value);
    logand_test "Logand 0x0001 0x0100 -> 0x0000" (UInt8.one) (UInt8.from_int 256) (UInt8.zero);
    logand_test "Logand 0x0100 0x0001 -> 0x0000" (UInt8.from_int 256) (UInt8.one) (UInt8.zero);
];;

let logor_test (name: string) (a: uint8) (b: uint8) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.logor a b) ~printer:UInt8.to_string
;;

let (logor_tests: test list) = [
    logor_test "Logor 0x0000 0x0000 -> 0x0000" (UInt8.zero) (UInt8.zero) (UInt8.zero);
    logor_test "Logor 0x0000 0x0001 -> 0x0001" (UInt8.zero) (UInt8.one) (UInt8.one);
    logor_test "Logor 0x0001 0x0000 -> 0x0001" (UInt8.one) (UInt8.zero) (UInt8.one);
    logor_test "Logor 0x0001 0x00FF -> 0x00FF" (UInt8.one) (UInt8.max_value) (UInt8.max_value);
    logor_test "Logor 0x00FF 0x00FF -> 0x00FF" (UInt8.max_value) (UInt8.max_value) (UInt8.max_value);
    logor_test "Logor 0x0001 0x0100 -> 0x0001" (UInt8.one) (UInt8.from_int 256) (UInt8.one);
    logor_test "Logor 0x0100 0x0001 -> 0x0001" (UInt8.from_int 256) (UInt8.one) (UInt8.one);
];;

let logxor_test (name: string) (a: uint8) (b: uint8) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.logxor a b) ~printer:UInt8.to_string
;;

let (logxor_tests: test list) = [
    logxor_test "Logxor 0x0000 0x0000 -> 0x0000" (UInt8.zero) (UInt8.zero) (UInt8.zero);
    logxor_test "Logxor 0x0000 0x0001 -> 0x0001" (UInt8.zero) (UInt8.one) (UInt8.one);
    logxor_test "Logxor 0x0001 0x0000 -> 0x0001" (UInt8.one) (UInt8.zero) (UInt8.one);
    logxor_test "Logxor 0x0001 0x00FF -> 0x00FE" (UInt8.one) (UInt8.max_value) (UInt8.from_int 254);
    logxor_test "Logxor 0x00FF 0x00FF -> 0x0000" (UInt8.max_value) (UInt8.max_value) (UInt8.zero);
    logxor_test "Logxor 0x0001 0x0100 -> 0x0001" (UInt8.one) (UInt8.from_int 256) (UInt8.one);
    logxor_test "Logxor 0x0100 0x0001 -> 0x0001" (UInt8.from_int 256) (UInt8.one) (UInt8.one);
];;

let shift_left_test (name: string) (a: uint8) (b: int) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.shift_left a b) ~printer:UInt8.to_string
;;

let (shift_left_tests: test list) = [
    shift_left_test "Shift_left 0x0000 0 -> 0x0000" (UInt8.zero) (0) (UInt8.zero);
    shift_left_test "Shift_left 0x0000 1 -> 0x0000" (UInt8.zero) (1) (UInt8.zero);
    shift_left_test "Shift_left 0x0001 0 -> 0x0001" (UInt8.one) (0) (UInt8.one);
    shift_left_test "Shift_left 0x0001 1 -> 0x0002" (UInt8.one) (1) (UInt8.from_int 2);
    shift_left_test "Shift_left 0x0001 7 -> 0x0080" (UInt8.one) (7) (UInt8.from_int 128);
    shift_left_test "Shift_left 0x0001 8 -> 0x0000" (UInt8.one) (8) (UInt8.zero);
    shift_left_test "Shift_left 0x00FF 1 -> 0x00FE" (UInt8.one) (9) (UInt8.zero);
];;

let shift_right_test (name: string) (a: uint8) (b: int) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.shift_right a b) ~printer:UInt8.to_string
;;

let (shift_right_tests: test list) = [
    shift_right_test "Shift_right 0x0000 0 -> 0x0000" (UInt8.zero) (0) (UInt8.zero);
    shift_right_test "Shift_right 0x0000 1 -> 0x0000" (UInt8.zero) (1) (UInt8.zero);
    shift_right_test "Shift_right 0x0001 0 -> 0x0001" (UInt8.one) (0) (UInt8.one);
    shift_right_test "Shift_right 0x0001 1 -> 0x0000" (UInt8.one) (1) (UInt8.zero);
    shift_right_test "Shift_right 0x0001 7 -> 0x0000" (UInt8.one) (7) (UInt8.zero);
    shift_right_test "Shift_right 0x0001 8 -> 0x0000" (UInt8.one) (8) (UInt8.zero);
    shift_right_test "Shift_right 0x00FF 1 -> 0x007F" (UInt8.max_value) (1) (UInt8.from_int 127);
    shift_right_test "Shift_right 0x00FF 7 -> 0x0001" (UInt8.max_value) (7) (UInt8.one);
];;

let from_int_test (name: string) (a: int) (expected: uint8) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.from_int a) ~printer:UInt8.to_string
;;

let (from_int_tests: test list) = [
    from_int_test "From_int 0 -> 0x0000" (0) (UInt8.zero);
    from_int_test "From_int 1 -> 0x0001" (1) (UInt8.one);
    from_int_test "From_int 255 -> 0x00FF" (255) (UInt8.max_value);
    from_int_test "From_int 256 -> 0x0000" (256) (UInt8.zero);
];;

let to_int_test (name: string) (a: uint8) (expected: int) : test =
    name >:: fun _ ->
        assert_equal expected (UInt8.to_int a) ~printer:string_of_int
;;

let (to_int_tests: test list) = [
    to_int_test "To_int 0x0000 -> 0" (UInt8.zero) (0);
    to_int_test "To_int 0x0001 -> 1" (UInt8.one) (1);
    to_int_test "To_int 0x00FF -> 255" (UInt8.max_value) (255);
];;

let (tests: test list) = List.flatten [
    to_string_tests; compare_tests; equal_tests; add_tests; sub_tests; mul_tests;
    div_tests; rem_tests; succ_tests; pred_tests; logand_tests; logor_tests;
    logxor_tests; shift_left_tests; shift_right_tests; from_int_tests; to_int_tests;
];;
