open Lib
open OUnit2
open Lib.Alias

(** [open_rom_test name rom exp] tests that [open_rom rom] doesn't raise an error. *)
let open_rom_test (name : string) (rom : string) =
    let _ = Util.open_rom rom in
    name >:: fun _ -> assert_bool "" true

(** Open_rom tests to run. *)
let open_rom_tests =
    [
        open_rom_test "Open_rom snake" "snake.nes";
        open_rom_test "Open_rom mario" "hidden/mario.nes";
    ]

(** [set_color_test name color] tests that [set_color color] doesn't raise an error. *)
let set_color_test (name : string) (color : uint8) =
    let _ = Graphics.open_graph "" in
    let _ = Util.set_color color in
    name >:: fun _ -> assert_bool "" true

(** Set_color tests to run. *)
let set_color_tests =
    [
        set_color_test "Set_color 0" ~.0;
        set_color_test "Set_color 1" ~.1;
        set_color_test "Set_color 2" ~.2;
        set_color_test "Set_color 3" ~.3;
        set_color_test "Set_color 4" ~.4;
        set_color_test "Set_color 5" ~.5;
        set_color_test "Set_color 6" ~.6;
        set_color_test "Set_color 7" ~.7;
        set_color_test "Set_color 8" ~.8;
        set_color_test "Set_color 9" ~.9;
        set_color_test "Set_color 10" ~.10;
        set_color_test "Set_color 11" ~.11;
        set_color_test "Set_color 12" ~.12;
        set_color_test "Set_color 13" ~.13;
        set_color_test "Set_color 14" ~.14;
        set_color_test "Set_color 15" ~.15;
        set_color_test "Set_color 15" ~.16;
    ]

let constant_tests =
    [
        ("PSize" >:: fun _ -> assert_bool "" (Util.psize > 0));
        ("SSize" >:: fun _ -> assert_bool "" (Util.ssize > 0));
        ("Stall" >:: fun _ -> assert_bool "" (Util.stall > 0));
    ]

(** All tests to run. *)
let tests : test list =
    List.flatten [ open_rom_tests; set_color_tests; constant_tests ]
