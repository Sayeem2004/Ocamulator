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

(** [bool_to_ui8 name b expected] tests equivalence between [?. b] and [expected]. *)
let bool_to_ui8 (name : string) (b : bool) (expected : UInt8.t) : test =
    name >:: fun _ -> assert_equal expected (?. b) ~printer:UInt8.to_string

(** Bool_to_ui8 tests to be run. *)
let bool_to_ui8_tests : test list =
    [
        bool_to_ui8 "Bool_to_ui8 false -> $00" false UInt8.zero;
        bool_to_ui8 "Bool_to_ui8 true -> $01" true UInt8.one;
    ]

(** [add_compare_test name a b expected] tests equivalence between [?> a b] and [expected]. *)
let add_compare_test (name : string) (a : uint8) (b : uint8) (expected : bool) : test =
    name >:: fun _ -> assert_equal expected (?> a b) ~printer:string_of_bool

(** Add_compare tests to be run. *)
let add_compare_tests : test list =
    [
        add_compare_test "Add_compare 0 0 -> false" UInt8.zero UInt8.zero false;
        add_compare_test "Add_compare 0 1 -> false" UInt8.zero UInt8.one false;
        add_compare_test "Add_compare 1 0 -> false" UInt8.one UInt8.zero false;
        add_compare_test "Add_compare 1 1 -> false" UInt8.one UInt8.one false;
        add_compare_test "Add_compare 128 128 -> true" (UInt8.from_int 128)
            (UInt8.from_int 128) true;
    ]

(** [check_most_bit_test name n expected] tests equivalence between [?- n] and [expected]. *)
let check_most_bit_test (name : string) (n : uint8) (expected : bool) : test =
    name >:: fun _ -> assert_equal expected (?- n) ~printer:string_of_bool

(** Check_most_bit tests to be run. *)
let check_most_bit_tests : test list =
    [
        check_most_bit_test "Check_most_bit 0 -> false" UInt8.zero false;
        check_most_bit_test "Check_most_bit 1 -> false" UInt8.one false;
        check_most_bit_test "Check_most_bit 128 -> true" (UInt8.from_int 128) true;
    ]

(** [check_zero_test name n expected] tests equivalence between [?* n] and [expected]. *)
let check_zero_test (name : string) (n : uint8) (expected : bool) : test =
    name >:: fun _ -> assert_equal expected (?* n) ~printer:string_of_bool

(** Check_zero tests to be run. *)
let check_zero_tests : test list =
    [
        check_zero_test "Check_zero 0 -> true" UInt8.zero true;
        check_zero_test "Check_zero 1 -> false" UInt8.one false;
    ]

(** [check_least_bit_test name n expected] tests equivalence between [?+ n] and [expected]. *)
let check_least_bit_test (name : string) (n : uint8) (expected : bool) : test =
    name >:: fun _ -> assert_equal expected (?+ n) ~printer:string_of_bool

(** Check_least_bit tests to be run. *)
let check_least_bit_tests : test list =
    [
        check_least_bit_test "Check_least_bit 0 -> false" UInt8.zero false;
        check_least_bit_test "Check_least_bit 1 -> true" UInt8.one true;
    ]

(** UInt8 tests to be run. *)
let (tests : test list) =
    UInt8Test.tests @ to_string_tests @ bool_to_ui8_tests @ add_compare_tests
    @ check_most_bit_tests @ check_zero_tests @ check_least_bit_tests
