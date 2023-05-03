open Lib
open OUnit2
open Lib.Alias

(** [UIntXTest.ml] contains coverage and accuracy tests for [UIntX.ml]. *)

(** Functor that tests a generic UInt type. *)
module Tester (M : UIntX.UInt) = struct
    (** [over] is an integer that is gauranteed to overflow an unsigned int. *)
    let over : int = M.to_int M.max + 1

    (** [compare_test name a b expected] tests equivalence between [compare a b]
        and [expected]. *)
    let compare_test (name : string) (a : M.t) (b : M.t) (expected : int) : test =
        name >:: fun _ ->
            assert_equal expected (M.compare a b) ~printer:string_of_int

    (** Compare tests to be run. *)
    let compare_tests : test list =
        [
            compare_test "Compare Zero Zero -> 0" M.zero M.zero 0;
            compare_test "Compare Zero Ones -> -1" M.zero M.one (-1);
            compare_test "Compare Ones Zero -> 1" M.one M.zero 1;
            compare_test "Compare Zero Maxx -> -1" M.zero M.max (-1);
            compare_test "Compare Maxx Ones -> 1" M.max M.one 1;
            compare_test "Compare Zero Over -> 0" M.zero (M.from_int over) 0;
            compare_test "Compare Over Ones -> -1" (M.from_int over) M.one (-1);
        ]

    (** [equal_test name a b expected] tests equivalence between [equal a b]
        and [expected]. *)
    let equal_test (name : string) (a : M.t) (b : M.t) (expected : bool) : test =
        name >:: fun _ ->
            assert_equal expected (M.equal a b) ~printer:string_of_bool

    (** Equal tests to be run. *)
    let equal_tests : test list =
        [
            equal_test "Equal Zero Zero -> true" M.zero M.zero true;
            equal_test "Equal Zero Ones -> false" M.zero M.one false;
            equal_test "Equal Ones Zero -> false" M.one M.zero false;
            equal_test "Equal Zero Maxx -> false" M.zero M.max false;
            equal_test "Equal Maxx Ones -> false" M.max M.one false;
            equal_test "Equal Zero Over -> true" M.zero (M.from_int over) true;
            equal_test "Equal Over Ones -> false" (M.from_int over) M.one false;
        ]

    (** [add_test name a b expected] tests equivalence between [add a b] and
        [expected]. *)
    let add_test (name : string) (a : M.t) (b : M.t) (expected : M.t) : test =
        name >:: fun _ -> assert_equal expected (M.add a b) ~printer:M.to_string

    (** Add tests to be run. *)
    let add_tests : test list =
        [
            add_test "Add Zero Zero -> Zero" M.zero M.zero M.zero;
            add_test "Add Zero Ones -> Ones" M.zero M.one M.one;
            add_test "Add Ones Zero -> Ones" M.one M.zero M.one;
            add_test "Add Zero Maxx -> Maxx" M.zero M.max M.max;
            add_test "Add Maxx Zero -> Maxx" M.max M.zero M.max;
            add_test "Add Zero Over -> Zero" M.zero (M.from_int over) M.zero;
            add_test "Add Over Zero -> Zero" (M.from_int over) M.zero M.zero;
        ]

    (** [sub_test name a b expected] tests equivalence between [sub a b] and
        [expected]. *)
    let sub_test (name : string) (a : M.t) (b : M.t) (expected : M.t) : test =
        name >:: fun _ -> assert_equal expected (M.sub a b) ~printer:M.to_string

    (** Sub tests to be run. *)
    let sub_tests : test list =
        [
            sub_test "Sub Zero Zero -> Zero" M.zero M.zero M.zero;
            sub_test "Sub Zero Ones -> Maxx" M.zero M.one M.max;
            sub_test "Sub Ones Zero -> Ones" M.one M.zero M.one;
            sub_test "Sub Zero Maxx -> Ones" M.zero M.max M.one;
            sub_test "Sub Maxx Zero -> Maxx" M.max M.zero M.max;
            sub_test "Sub Zero Over -> Zero" M.zero (M.from_int over) M.zero;
            sub_test "Sub Over Zero -> Zero" (M.from_int over) M.zero M.zero;
        ]

    (** [mul_test name a b expected] tests equivalence between [mul a b] and
        [expected]. *)
    let mul_test (name : string) (a : M.t) (b : M.t) (expected : M.t) : test =
        name >:: fun _ -> assert_equal expected (M.mul a b) ~printer:M.to_string

    (** Mul tests to be run. *)
    let mul_tests : test list =
        [
            mul_test "Mul Zero Zero -> Zero" M.zero M.zero M.zero;
            mul_test "Mul Zero Ones -> Zero" M.zero M.one M.zero;
            mul_test "Mul Ones Zero -> Zero" M.one M.zero M.zero;
            mul_test "Mul Ones Maxx -> Maxx" M.one M.max M.max;
            mul_test "Mul Maxx Ones -> Maxx" M.max M.one M.max;
            mul_test "Mul Ones Over -> Zero" M.one (M.from_int over) M.zero;
            mul_test "Mul Over Ones -> Zero" (M.from_int over) M.one M.zero;
        ]

    (** [div_test name a b expected] tests equivalence between [div a b] and
        [expected]. *)
    let div_test (name : string) (a : M.t) (b : M.t) (expected : M.t) : test =
        name >:: fun _ -> assert_equal expected (M.div a b) ~printer:M.to_string

    (** Div tests to be run. *)
    let div_tests : test list =
        [
            div_test "Div Zero Ones -> Zero" M.zero M.one M.zero;
            div_test "Div Ones Maxx -> Zero" M.one M.max M.zero;
            div_test "Div Maxx Ones -> Maxx" M.max M.one M.max;
            div_test "Div Over Ones -> Zero" (M.from_int over) M.one M.zero;
        ]

    (** [rem_test name a b expected] tests equivalence between [rem a b] and
        [expected]. *)
    let rem_test (name : string) (a : M.t) (b : M.t) (expected : M.t) : test =
        name >:: fun _ -> assert_equal expected (M.rem a b) ~printer:M.to_string

    (** Rem tests to be run. *)
    let rem_tests : test list =
        [
            rem_test "Rem Zero Ones -> Zero" M.zero M.one M.zero;
            rem_test "Rem Ones Maxx -> Ones" M.one M.max M.one;
            rem_test "Rem Maxx Ones -> Zero" M.max M.one M.zero;
            rem_test "Rem Over Ones -> Zero" (M.from_int over) M.one M.zero;
        ]

    (** [succ_test name a expected] tests equivalence between [succ a] and
        [expected] *)
    let succ_test (name : string) (a : M.t) (expected : M.t) : test =
        name >:: fun _ -> assert_equal expected (M.succ a) ~printer:M.to_string

    (** Succ tests to be run. *)
    let succ_tests : test list =
        [
            succ_test "Succ Zero -> Ones" M.zero M.one;
            succ_test "Succ Maxx -> Zero" M.max M.zero;
        ]

    (** [pred_test name a expected] tests equivalence between [pred a] and
        [expected] *)
    let pred_test (name : string) (a : M.t) (expected : M.t) : test =
        name >:: fun _ -> assert_equal expected (M.pred a) ~printer:M.to_string

    (** Pred tests to be run. *)
    let pred_tests : test list =
        [
            pred_test "Pred Ones -> Zero" M.one M.zero;
            pred_test "Pred Zero -> Maxx" M.zero M.max;
        ]

    (** [logand_test name a b expected] tests equivalence between [logand a b] and
        [expected]. *)
    let logand_test (name : string) (a : M.t) (b : M.t) (expected : M.t) : test =
        name >:: fun _ -> assert_equal expected (M.logand a b) ~printer:M.to_string

    (** Logand tests to be run. *)
    let logand_tests : test list =
        [
            logand_test "Logand Zero Zero -> Zero" M.zero M.zero M.zero;
            logand_test "Logand Zero Ones -> Zero" M.zero M.one M.zero;
            logand_test "Logand Ones Zero -> Zero" M.one M.zero M.zero;
            logand_test "Logand Ones Maxx -> Ones" M.one M.max M.one;
            logand_test "Logand Maxx Maxx -> Maxx" M.max M.max M.max;
            logand_test "Logand Ones Over -> Zero" M.one (M.from_int over) M.zero;
            logand_test "Logand Over Ones -> Zero" (M.from_int over) M.one M.zero;
        ]

    (** [logor_test name a b expected] tests equivalence between [logor a b] and
        [expected]. *)
    let logor_test (name : string) (a : M.t) (b : M.t) (expected : M.t) : test =
        name >:: fun _ -> assert_equal expected (M.logor a b) ~printer:M.to_string

    (** Logor tests to be run. *)
    let logor_tests : test list =
        [
            logor_test "Logor Zero Zero -> Zero" M.zero M.zero M.zero;
            logor_test "Logor Zero Ones -> Ones" M.zero M.one M.one;
            logor_test "Logor Ones Zero -> Ones" M.one M.zero M.one;
            logor_test "Logor Ones Maxx -> Maxx" M.one M.max M.max;
            logor_test "Logor Maxx Maxx -> Maxx" M.max M.max M.max;
            logor_test "Logor Ones Over -> Ones" M.one (M.from_int over) M.one;
            logor_test "Logor Over Ones -> Ones" (M.from_int over) M.one M.one;
        ]

    (** [logxor_test name a b expected] tests equivalence between [logxor a b] and
        [expected]. *)
    let logxor_test (name : string) (a : M.t) (b : M.t) (expected : M.t) : test =
        name >:: fun _ -> assert_equal expected (M.logxor a b) ~printer:M.to_string

    (** Logxor tests to be run. *)
    let logxor_tests : test list =
        [
            logxor_test "Logxor Zero Zero -> Zero" M.zero M.zero M.zero;
            logxor_test "Logxor Zero Ones -> Ones" M.zero M.one M.one;
            logxor_test "Logxor Ones Zero -> Ones" M.one M.zero M.one;
            logxor_test "Logxor Ones Maxx -> Maxx - 1" M.one M.max (M.pred M.max);
            logxor_test "Logxor Maxx Maxx -> Zero" M.max M.max M.zero;
            logxor_test "Logxor Ones Over -> Ones" M.one (M.from_int over) M.one;
            logxor_test "Logxor Over Ones -> Ones" (M.from_int over) M.one M.one;
        ]

    (** [shift_left_test name a b expected] tests equivalence between
        [shift_left a b] and [expected]. *)
    let shift_left_test (name : string) (a : M.t) (b : int) (expected : M.t) :
        test =
        name >:: fun _ -> assert_equal expected (M.left a b) ~printer:M.to_string

    (** Shift_left tests to be run. *)
    let shift_left_tests : test list =
        [
            shift_left_test "Shift_left Zero 0 -> Zero" M.zero 0 M.zero;
            shift_left_test "Shift_left Zero 1 -> Zero" M.zero 1 M.zero;
            shift_left_test "Shift_left Ones 0 -> One" M.one 0 M.one;
            shift_left_test "Shift_left Ones 1 -> Two" M.one 1 (M.succ M.one);
            shift_left_test "Shift_left Ones Size - 1 -> Over / 2" M.one (M.size - 1)
                (M.from_int (over / 2));
            shift_left_test "Shift_left Ones Size -> Over" M.one M.size
                (M.from_int over);
            shift_left_test "Shift_left Maxx 1 -> Maxx - 1" M.max 1 (M.pred M.max);
        ]

    (** [shift_right_test name a b expected] tests equivalence between
        [shift_right a b] and [expected]. *)
    let shift_right_test (name : string) (a : M.t) (b : int) (expected : M.t) :
        test =
        name >:: fun _ -> assert_equal expected (M.right a b) ~printer:M.to_string

    (** Shift_right tests to be run. *)
    let shift_right_tests : test list =
        [
            shift_right_test "Shift_right Zero 0 -> Zero" M.zero 0 M.zero;
            shift_right_test "Shift_right Zero 1 -> Zero" M.zero 1 M.zero;
            shift_right_test "Shift_right Ones 0 -> Ones" M.one 0 M.one;
            shift_right_test "Shift_right Ones 1 -> Zero" M.one 1 M.zero;
            shift_right_test "Shift_right Ones Size - 1 -> Zero" M.one (M.size - 1)
                M.zero;
            shift_right_test "Shift_right Ones Size -> Zero" M.one M.size M.zero;
            shift_right_test "Shift_right Maxx 1 -> Maxx / 2" M.max 1
                (M.from_int ((over - 1) / 2));
            shift_right_test "Shift_right Maxx Size - 1 -> Ones" M.max (M.size - 1)
                M.one;
        ]

    (** [from_int_test name a expected] tests equivalence between [from_int a] and
        [expected]. *)
    let from_int_test (name : string) (a : int) (expected : M.t) : test =
        name >:: fun _ -> assert_equal expected (M.from_int a) ~printer:M.to_string

    (** From_int tests to be run. *)
    let from_int_tests : test list =
        [
            from_int_test "From_int Zero -> Zero" 0 M.zero;
            from_int_test "From_int Ones -> Ones" 1 M.one;
            from_int_test "From_int Maxx -> Maxx" (over - 1) M.max;
            from_int_test "From_int Over -> Zero" over M.zero;
        ]

    (** [to_int_test name a expected] tests equivalence between [to_int a] and
        [expected]. *)
    let to_int_test (name : string) (a : M.t) (expected : int) : test =
        name >:: fun _ -> assert_equal expected (M.to_int a) ~printer:string_of_int

    (** To_int tests to be run. *)
    let to_int_tests : test list =
        [
            to_int_test "To_int Zero -> 0" M.zero 0;
            to_int_test "To_int Ones -> 1" M.one 1;
            to_int_test "To_int Maxx -> Maxx" M.max (over - 1);
        ]

    (** [to_string_test name a expected] tests equivalence between [to_string a]
        and [expected]. *)
    let to_string_test (name : string) (input : M.t) (expected : string) : test =
        name >:: fun _ ->
            assert_equal expected (M.to_string input) ~printer:(fun x -> x)

    (** UIntX tests to be run. *)
    let tests : test list =
        List.flatten
            [
                compare_tests;
                equal_tests;
                add_tests;
                sub_tests;
                mul_tests;
                div_tests;
                rem_tests;
                succ_tests;
                pred_tests;
                logand_tests;
                logor_tests;
                logxor_tests;
                shift_left_tests;
                shift_right_tests;
                from_int_tests;
                to_int_tests;
            ]
end
