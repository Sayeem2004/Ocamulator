open OUnit2

(** Ocamulator tests to be run. *)
let (tests : test) =
    "Test Suite For Ocamulator"
    >::: List.flatten
        [ UInt8Test.tests; UInt16Test.tests; CpuTest.tests; ]

(** Running tests. *)
let _ = run_test_tt_main tests
