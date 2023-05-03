open Lib
open OUnit2
open Lib.Alias

(** [bool_to_ui8_test name b exp] tests equivalence between [?. b] and [exp]. *)
let bool_to_ui8_test (name : string) (b : bool) (exp : uint8) : test =
    name >:: fun _ -> assert_equal exp ?.b ~printer:UInt8.to_string

(** Bool_to_ui8 tests to run. *)
let bool_to_ui8_tests : test list =
    [
        bool_to_ui8_test "bool_to_ui8 false" false ~.0x00;
        bool_to_ui8_test "bool_to_ui8 true" true ~.0x01;
    ]

(** [check_zero_test name a exp] tests equivalence between [?* a] and [exp]. *)
let check_zero_test (name : string) (a : uint8) (exp : bool) : test =
    name >:: fun _ -> assert_equal exp ?*a ~printer:string_of_bool

(** Check_zero tests to run. *)
let check_zero_tests : test list =
    [
        check_zero_test "check_zero zero" ~.0x00 true;
        check_zero_test "check_zero non-zero 1" ~.0x01 false;
        check_zero_test "check_zero non-zero 2" ~.0x80 false;
    ]

(** [check_high_bit_test name a exp] tests equivalence between [?- a] and [exp]. *)
let check_high_bit_test (name : string) (a : uint8) (exp : bool) : test =
    name >:: fun _ -> assert_equal exp ?-a ~printer:string_of_bool

(** Check_high_bit tests to run. *)
let check_high_bit_tests : test list =
    [
        check_high_bit_test "check_high_bit zero" ~.0x00 false;
        check_high_bit_test "check_high_bit non-zero 1" ~.0x01 false;
        check_high_bit_test "check_high_bit non-zero 2" ~.0x80 true;
    ]

(** [check_low_bit_test name a exp] tests equivalence between [?+ a] and [exp]. *)
let check_low_bit_test (name : string) (a : uint8) (exp : bool) : test =
    name >:: fun _ -> assert_equal exp ?+a ~printer:string_of_bool

(** Check_low_bit tests to run. *)
let check_low_bit_tests : test list =
    [
        check_low_bit_test "check_low_bit zero" ~.0x00 false;
        check_low_bit_test "check_low_bit non-zero 1" ~.0x01 true;
        check_low_bit_test "check_low_bit non-zero 2" ~.0x80 false;
    ]

(** [twos_complement_test name a exp] tests equivalence between [?@ a] and [exp]. *)
let twos_complement_test (name : string) (a : uint8) (exp : uint8) : test =
    name >:: fun _ -> assert_equal exp ?@a ~printer:UInt8.to_string

(** Twos_complement tests to run. *)
let twos_complement_tests : test list =
    [
        twos_complement_test "twos_complement zero" ~.0x00 ~.0xFF;
        twos_complement_test "twos_complement non-zero 1" ~.0x01 ~.0xFE;
        twos_complement_test "twos_complement non-zero 2" ~.0x80 ~.0x7F;
    ]

(** [to_signed_test name a exp] tests equivalence between [?% a] and [exp]. *)
let to_signed_test (name : string) (a : uint8) (exp : int) : test =
    name >:: fun _ -> assert_equal exp ?%a ~printer:string_of_int

(** To_signed tests to run. *)
let to_signed_tests : test list =
    [
        to_signed_test "to_signed zero" ~.0x00 0;
        to_signed_test "to_signed non-zero 1" ~.0x01 1;
        to_signed_test "to_signed non-zero 2" ~.0x80 (-128);
    ]

(** [ui16_to_ui8_test name a exp] tests equivalence between [!-- a] and [exp]. *)
let ui16_to_ui8_test (name : string) (a : uint16) (exp : uint8) : test =
    name >:: fun _ -> assert_equal exp !--a ~printer:UInt8.to_string

(** Ui16_to_ui8 tests to run. *)
let ui16_to_ui8_tests : test list =
    [
        ui16_to_ui8_test "ui16_to_ui8 zero" ~..0x0000 ~.0x00;
        ui16_to_ui8_test "ui16_to_ui8 non-zero 1" ~..0x0001 ~.0x01;
        ui16_to_ui8_test "ui16_to_ui8 non-zero 2" ~..0x8000 ~.0x00;
    ]

(** [bool_to_ui16_test name b exp] tests equivalence between [!++ b] and [exp]. *)
let bool_to_ui16 (name : string) (b : bool) (exp : uint16) : test =
    name >:: fun _ -> assert_equal exp !++b ~printer:UInt16.to_string

(** Bool_to_ui16 tests to run. *)
let bool_to_ui16_tests : test list =
    [
        bool_to_ui16 "bool_to_ui16 false" false ~..0x0000;
        bool_to_ui16 "bool_to_ui16 true" true ~..0x0001;
    ]

(** All tests to run. *)
let tests : test list =
    List.flatten
        [
            bool_to_ui8_tests;
            check_zero_tests;
            check_high_bit_tests;
            check_low_bit_tests;
            twos_complement_tests;
            to_signed_tests;
            ui16_to_ui8_tests;
            bool_to_ui16_tests;
        ]
