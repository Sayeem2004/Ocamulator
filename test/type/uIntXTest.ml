open OUnit2
open Lib__UInt

module F (M: S) = struct
    let (over: int) = (M.to_int M.max_value) + 1;;

    let compare_test (name: string) (a: M.t) (b: M.t) (expected: int) : test =
        name >:: fun _ ->
            assert_equal expected (M.compare a b) ~printer:string_of_int
    ;;

    let (compare_tests: test list) = [
        compare_test "Compare Zero Zero -> 0" (M.zero) (M.zero) (0);
        compare_test "Compare Zero Ones -> -1" (M.zero) (M.one) (-1);
        compare_test "Compare Ones Zero -> 1" (M.one) (M.zero) (1);
        compare_test "Compare Zero Maxx -> -1" (M.zero) (M.max_value) (-1);
        compare_test "Compare Maxx Ones -> 1" (M.max_value) (M.one) (1);
        compare_test "Compare Zero Over -> 0" (M.zero) (M.from_int over) (0);
        compare_test "Compare Over Ones -> -1" (M.from_int over) (M.one) (-1);
    ];;

    let equal_test (name: string) (a: M.t) (b: M.t) (expected: bool) : test =
        name >:: fun _ ->
            assert_equal expected (M.equal a b) ~printer:string_of_bool
    ;;

    let (equal_tests: test list) = [
        equal_test "Equal Zero Zero -> true" (M.zero) (M.zero) (true);
        equal_test "Equal Zero Ones -> false" (M.zero) (M.one) (false);
        equal_test "Equal Ones Zero -> false" (M.one) (M.zero) (false);
        equal_test "Equal Zero Maxx -> false" (M.zero) (M.max_value) (false);
        equal_test "Equal Maxx Ones -> false" (M.max_value) (M.one) (false);
        equal_test "Equal Zero Over -> true" (M.zero) (M.from_int over) (true);
        equal_test "Equal Over Ones -> false" (M.from_int over) (M.one) (false);
    ];;

    let add_test (name: string) (a: M.t) (b: M.t) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.add a b) ~printer:M.to_string
    ;;

    let (add_tests: test list) = [
        add_test "Add Zero Zero -> Zero" (M.zero) (M.zero) (M.zero);
        add_test "Add Zero Ones -> Ones" (M.zero) (M.one) (M.one);
        add_test "Add Ones Zero -> Ones" (M.one) (M.zero) (M.one);
        add_test "Add Zero Maxx -> Maxx" (M.zero) (M.max_value) (M.max_value);
        add_test "Add Maxx Zero -> Maxx" (M.max_value) (M.zero) (M.max_value);
        add_test "Add Zero Over -> Zero" (M.zero) (M.from_int over) (M.zero);
        add_test "Add Over Zero -> Zero" (M.from_int over) (M.zero) (M.zero);
    ];;

    let sub_test (name: string) (a: M.t) (b: M.t) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.sub a b) ~printer:M.to_string
    ;;

    let (sub_tests: test list) = [
        sub_test "Sub Zero Zero -> Zero" (M.zero) (M.zero) (M.zero);
        sub_test "Sub Zero Ones -> Maxx" (M.zero) (M.one) (M.max_value);
        sub_test "Sub Ones Zero -> Ones" (M.one) (M.zero) (M.one);
        sub_test "Sub Zero Maxx -> Ones" (M.zero) (M.max_value) (M.one);
        sub_test "Sub Maxx Zero -> Maxx" (M.max_value) (M.zero) (M.max_value);
        sub_test "Sub Zero Over -> Zero" (M.zero) (M.from_int over) (M.zero);
        sub_test "Sub Over Zero -> Zero" (M.from_int over) (M.zero) (M.zero);
    ];;

    let mul_test (name: string) (a: M.t) (b: M.t) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.mul a b) ~printer:M.to_string
    ;;

    let (mul_tests: test list) = [
        mul_test "Mul Zero Zero -> Zero" (M.zero) (M.zero) (M.zero);
        mul_test "Mul Zero Ones -> Zero" (M.zero) (M.one) (M.zero);
        mul_test "Mul Ones Zero -> Zero" (M.one) (M.zero) (M.zero);
        mul_test "Mul Ones Maxx -> Maxx" (M.one) (M.max_value) (M.max_value);
        mul_test "Mul Maxx Ones -> Maxx" (M.max_value) (M.one) (M.max_value);
        mul_test "Mul Ones Over -> Zero" (M.one) (M.from_int over) (M.zero);
        mul_test "Mul Over Ones -> Zero" (M.from_int over) (M.one) (M.zero);
    ];;

    let div_test (name: string) (a: M.t) (b: M.t) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.div a b) ~printer:M.to_string
    ;;

    let (div_tests: test list) = [
        div_test "Div Zero Ones -> Zero" (M.zero) (M.one) (M.zero);
        div_test "Div Ones Maxx -> Zero" (M.one) (M.max_value) (M.zero);
        div_test "Div Maxx Ones -> Maxx" (M.max_value) (M.one) (M.max_value);
        div_test "Div Over Ones -> Zero" (M.from_int over) (M.one) (M.zero);
    ];;

    let rem_test (name: string) (a: M.t) (b: M.t) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.rem a b) ~printer:M.to_string
    ;;

    let (rem_tests: test list) = [
        rem_test "Rem Zero Ones -> Zero" (M.zero) (M.one) (M.zero);
        rem_test "Rem Ones Maxx -> Ones" (M.one) (M.max_value) (M.one);
        rem_test "Rem Maxx Ones -> Zero" (M.max_value) (M.one) (M.zero);
        rem_test "Rem Over Ones -> Zero" (M.from_int over) (M.one) (M.zero);
    ];;

    let succ_test (name: string) (a: M.t) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.succ a) ~printer:M.to_string
    ;;

    let (succ_tests: test list) = [
        succ_test "Succ Zero -> Ones" (M.zero) (M.one);
        succ_test "Succ Maxx -> Zero" (M.max_value) (M.zero);
    ];;

    let pred_test (name: string) (a: M.t) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.pred a) ~printer:M.to_string
    ;;

    let (pred_tests: test list) = [
        pred_test "Pred Ones -> Zero" (M.one) (M.zero);
        pred_test "Pred Zero -> Maxx" (M.zero) (M.max_value);
    ];;

    let logand_test (name: string) (a: M.t) (b: M.t) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.logand a b) ~printer:M.to_string
    ;;

    let (logand_tests: test list) = [
        logand_test "Logand Zero Zero -> Zero" (M.zero) (M.zero) (M.zero);
        logand_test "Logand Zero Ones -> Zero" (M.zero) (M.one) (M.zero);
        logand_test "Logand Ones Zero -> Zero" (M.one) (M.zero) (M.zero);
        logand_test "Logand Ones Maxx -> Ones" (M.one) (M.max_value) (M.one);
        logand_test "Logand Maxx Maxx -> Maxx" (M.max_value) (M.max_value) (M.max_value);
        logand_test "Logand Ones Over -> Zero" (M.one) (M.from_int over) (M.zero);
        logand_test "Logand Over Ones -> Zero" (M.from_int over) (M.one) (M.zero);
    ];;

    let logor_test (name: string) (a: M.t) (b: M.t) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.logor a b) ~printer:M.to_string
    ;;

    let (logor_tests: test list) = [
        logor_test "Logor Zero Zero -> Zero" (M.zero) (M.zero) (M.zero);
        logor_test "Logor Zero Ones -> Ones" (M.zero) (M.one) (M.one);
        logor_test "Logor Ones Zero -> Ones" (M.one) (M.zero) (M.one);
        logor_test "Logor Ones Maxx -> Maxx" (M.one) (M.max_value) (M.max_value);
        logor_test "Logor Maxx Maxx -> Maxx" (M.max_value) (M.max_value) (M.max_value);
        logor_test "Logor Ones Over -> Ones" (M.one) (M.from_int over) (M.one);
        logor_test "Logor Over Ones -> Ones" (M.from_int over) (M.one) (M.one);
    ];;

    let logxor_test (name: string) (a: M.t) (b: M.t) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.logxor a b) ~printer:M.to_string
    ;;

    let (logxor_tests: test list) = [
        logxor_test "Logxor Zero Zero -> Zero" (M.zero) (M.zero) (M.zero);
        logxor_test "Logxor Zero Ones -> Ones" (M.zero) (M.one) (M.one);
        logxor_test "Logxor Ones Zero -> Ones" (M.one) (M.zero) (M.one);
        logxor_test "Logxor Ones Maxx -> Maxx - 1" (M.one) (M.max_value) (M.pred M.max_value);
        logxor_test "Logxor Maxx Maxx -> Zero" (M.max_value) (M.max_value) (M.zero);
        logxor_test "Logxor Ones Over -> Ones" (M.one) (M.from_int over) (M.one);
        logxor_test "Logxor Over Ones -> Ones" (M.from_int over) (M.one) (M.one);
    ];;

    let shift_left_test (name: string) (a: M.t) (b: int) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.shift_left a b) ~printer:M.to_string
    ;;

    let (shift_left_tests: test list) = [
        shift_left_test "Shift_left Zero 0 -> Zero" (M.zero) (0) (M.zero);
        shift_left_test "Shift_left Zero 1 -> Zero" (M.zero) (1) (M.zero);
        shift_left_test "Shift_left Ones 0 -> One" (M.one) (0) (M.one);
        shift_left_test "Shift_left Ones 1 -> Two" (M.one) (1) (M.succ M.one);
        shift_left_test "Shift_left Ones Size - 1 -> Over / 2" (M.one) (M.size - 1) (M.from_int (over / 2));
        shift_left_test "Shift_left Ones Size -> Over" (M.one) (M.size) (M.from_int over);
        shift_left_test "Shift_left Maxx 1 -> Maxx - 1" (M.max_value) (1) (M.pred M.max_value);
    ];;

    let shift_right_test (name: string) (a: M.t) (b: int) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.shift_right a b) ~printer:M.to_string
    ;;

    let (shift_right_tests: test list) = [
        shift_right_test "Shift_right Zero 0 -> Zero" (M.zero) (0) (M.zero);
        shift_right_test "Shift_right Zero 1 -> Zero" (M.zero) (1) (M.zero);
        shift_right_test "Shift_right Ones 0 -> Ones" (M.one) (0) (M.one);
        shift_right_test "Shift_right Ones 1 -> Zero" (M.one) (1) (M.zero);
        shift_right_test "Shift_right Ones Size - 1 -> Zero" (M.one) (M.size - 1) (M.zero);
        shift_right_test "Shift_right Ones Size -> Zero" (M.one) (M.size) (M.zero);
        shift_right_test "Shift_right Maxx 1 -> Maxx / 2" (M.max_value) (1) (M.from_int ((over - 1) / 2));
        shift_right_test "Shift_right Maxx Size - 1 -> Ones" (M.max_value) (M.size - 1) (M.one);
    ];;

    let from_int_test (name: string) (a: int) (expected: M.t) : test =
        name >:: fun _ ->
            assert_equal expected (M.from_int a) ~printer:M.to_string
    ;;

    let (from_int_tests: test list) = [
        from_int_test "From_int Zero -> Zero" (0) (M.zero);
        from_int_test "From_int Ones -> Ones" (1) (M.one);
        from_int_test "From_int Maxx -> Maxx" (over - 1) (M.max_value);
        from_int_test "From_int Over -> Zero" (over) (M.zero);
    ];;

    let to_int_test (name: string) (a: M.t) (expected: int) : test =
        name >:: fun _ ->
            assert_equal expected (M.to_int a) ~printer:string_of_int
    ;;

    let (to_int_tests: test list) = [
        to_int_test "To_int Zero -> 0" (M.zero) (0);
        to_int_test "To_int Ones -> 1" (M.one) (1);
        to_int_test "To_int Maxx -> Maxx" (M.max_value) (over - 1);
    ];;

    let to_string_test (name: string) (input: M.t) (expected: string) : test =
        name >:: fun _ ->
            assert_equal expected (M.to_string input) ~printer:(fun x -> x)
    ;;

    let (tests: test list) = List.flatten [
        compare_tests; equal_tests; add_tests; sub_tests; mul_tests; div_tests;
        rem_tests; succ_tests; pred_tests; logand_tests; logor_tests; logxor_tests;
        shift_left_tests; shift_right_tests; from_int_tests; to_int_tests;
    ];;
end
