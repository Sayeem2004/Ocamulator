open OUnit2

let (tests : test) =
    "Test Suite For NES Emulator"
    >::: List.flatten
        [ UInt8Test.tests; UInt16Test.tests; CpuTest.tests; RamTest.tests ]

let _ = run_test_tt_main tests
