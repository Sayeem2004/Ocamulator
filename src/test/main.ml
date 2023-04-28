open OUnit2

(** Ocamulator tests to be run. *)
let tests : test =
    "Test Suite For Ocamulator"
    >::: List.flatten
        [
            UInt8Test.tests;
            UInt16Test.tests;
            CpuTest.tests;
            DecodeTest.tests;
            InstructionTest.tests;
            OpcodeTest.tests;
            RamTest.tests;
        ]

(** Running tests. *)
let (_ : unit) = run_test_tt_main tests
