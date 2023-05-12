open OUnit2

(** [main.ml] contains coverage and accuracy tests for the emulator. *)

(** Ocamulator tests to be run. *)
let tests : test =
    "Test Suite For Ocamulator"
    >::: List.flatten
        [
            CpuTest.tests;
            RamTest.tests;
            DecodeTest.tests;
            InstructionTest.tests;
            OpcodeTest.tests;
            UInt8Test.tests;
            UInt16Test.tests;
            AliasTest.tests;
            UtilTest.tests;
        ]

(** Running tests. *)
let (_ : unit) = run_test_tt_main tests
