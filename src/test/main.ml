(* open OUnit2

(** [main.ml] contains coverage and accuracy tests for the emulator. *)

(** Test Plan:

    Our goal for this test suite was to be as expansive as possible
    leaving no corner unturned. To this end, we attempted to test every single
    file except the executables (Execute.ml and Simulate.ml). We also ignored
    testing functions that required GUI's to be opened. This was because we
    tested them manually through play testing instead. So everything but
    executables and graphical functions were automatically tested by OUnit,
    while the exceptions were tested manually.

    Our initial tests for each module were heavily black box tested because we
    wanted to be assured that the functionality resembled the actual NES
    hardware. After this initial phase of testing, we implemented the strategy
    of glass box testing to achieve 100% coverage in all files (again
    excluding graphical functions and executables). After writing our
    own tests, we also found external tests written for NES 6502 emulators by
    people that already had done similar projects in the past. Finally, after
    our initial system was finished, we play tested among ourselves, friends,
    and project mentors. We did not use randomized testing because we deemed
    randomly creating CPU tests to be difficult without much benefit as all
    we would have been able to confirm is that our system did not crash, not its
    correctness.

    We believe that our test suite is comprehensive because we have achieved
    100% coverage throughout our system. We have not only written ~800 tests by
    hand but also used ~6400 tests from external sources (Tom Harte's Processor
    Tests). With this combination of tests, we believe that we are dutily
    following the hardware specifications of the NES 6502. We also believe that
    if there were any bugs in our system, they would be minor enough to not
    affect the overall functionality of the system. Past our test suite, we have
    played games on our emulator and have tried to break it using random key
    combinations and other methods without success. All in all, we believe that
    our test suite is as comprehensive and robust as our system itself, and thus
    we are confident in the correctness of our system. *)

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
let (_ : unit) = run_test_tt_main tests *)
