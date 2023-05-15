open Ocamulator
open OUnit2
open Ocamulator.Alias

(** [UtilTest.ml] contains coverage and accuracy tests for [Util.ml]. *)

(** [open_rom_test name rom exp] tests that [open_rom rom] doesn't raise an error. *)
let open_rom_test (name : string) (rom : string) =
    let _ = Util.open_rom rom in
    name >:: fun _ -> assert_bool "" true

(** Open_rom tests to run. *)
let open_rom_tests = [ open_rom_test "Open_rom snake" "snake.nes" ]

let constant_tests =
    [
        ("PSize" >:: fun _ -> assert_bool "" (Util.psize > 0));
        ("SSize" >:: fun _ -> assert_bool "" (Util.ssize > 0));
        ("Stall" >:: fun _ -> assert_bool "" (Util.stall > 0));
    ]

(** All tests to run. *)
let tests : test list = List.flatten [ open_rom_tests; constant_tests ]
