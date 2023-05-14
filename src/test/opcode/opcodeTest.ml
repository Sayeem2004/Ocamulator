open Ocamulator
open OUnit2
open Ocamulator.Alias
open Yojson.Basic

(** [OpcodeTest.ml] contains coverage and accuracy tests for [Opcode.ml]. *)

type json = Yojson.Basic.t
(** Alias for Yojson.Basic.t type. *)

type cycle = int * int * string
(** Type representing a cycle in the opcode tests. *)

type rinfo = (int * int) list
(** Type representing important ram information in the opcode tests. *)

type opcode_test = {
    name : string;
    initial_state : Cpu.t;
    initial_ram : rinfo;
    final_state : Cpu.t;
    final_ram : rinfo;
    cycles : cycle list;
}
(** Type representing an opcode_test stored in data/opcode. *)

(** [cpu_from_json json] converts the given [json] into a cpu if possible. *)
let cpu_from_json (json : json) : Cpu.t =
    let prog_cnt = json |> Util.member "pc" |> Util.to_int |> ( ~.. ) in
    let stck_ptr = json |> Util.member "s" |> Util.to_int |> ( ~. ) in
    let acc = json |> Util.member "a" |> Util.to_int |> ( ~. ) in
    let reg_x = json |> Util.member "x" |> Util.to_int |> ( ~. ) in
    let reg_y = json |> Util.member "y" |> Util.to_int |> ( ~. ) in
    let flags = json |> Util.member "p" |> Util.to_int |> ( ~. ) in
    Cpu.spec_cpu prog_cnt stck_ptr acc reg_x reg_y flags (Ram.zero_ram ())

(** [cycles_from_json json] converts the given [json] into a list of cycles if
    possible. *)
let cycles_from_json (json : json) : cycle list =
    let json_list : json list = Util.to_list json in
    let parse (json : json) : cycle =
        let val_list = Util.to_list json in
        let addr : int = List.nth val_list 0 |> Util.to_int in
        let value : int = List.nth val_list 1 |> Util.to_int in
        let name : string = List.nth val_list 2 |> Util.to_string in
        (addr, value, name)
    in
    List.map parse json_list

(** [rinfo_from_json json] converts the given [json] into a [rinfo] if possible. *)
let rinfo_from_json (json : json) : rinfo =
    let ram : json = Util.member "ram" json in
    let json_list : json list = Util.to_list ram in
    let parse (json : json) : int * int =
        let val_list = Util.to_list json in
        let addr : int = List.nth val_list 0 |> Util.to_int in
        let value : int = List.nth val_list 1 |> Util.to_int in
        (addr, value)
    in
    List.map parse json_list

let apply_ram (ram : Ram.t) (info : rinfo) : unit =
    let apply (ram : Ram.t) ((addr, value) : int * int) : unit =
        Ram.write_ui8 ram ~..addr ~.value
    in
    List.iter (apply ram) info

(** [opcode_test_from_json json] converts the given [json] into an [opcode_test]
    if possible. *)
let opcode_test_from_json (json : json) : opcode_test =
    let name : string = json |> Util.member "name" |> to_string in
    let initial_state : Cpu.t = json |> Util.member "initial" |> cpu_from_json in
    let initial_ram : rinfo = json |> Util.member "initial" |> rinfo_from_json in
    let _ = apply_ram initial_state.ram initial_ram in
    let final_state : Cpu.t = json |> Util.member "final" |> cpu_from_json in
    let final_ram : rinfo = json |> Util.member "final" |> rinfo_from_json in
    let _ = apply_ram final_state.ram final_ram in
    let cycles : cycle list = json |> Util.member "cycles" |> cycles_from_json in
    { name; initial_state; initial_ram; final_state; final_ram; cycles }

(** [from_json json] converts the given [json] into an [opcode_test] list if
    possible. *)
let from_json (json : json) : opcode_test list =
    let try_json (json : json) : opcode_test option =
        try Some (opcode_test_from_json json) with _ -> None
    in
    List.filter_map try_json (Util.to_list json)

(** Converts the json file corresponding to the given opcode into a json type. *)
let parse_json (opcode : uint8) : json =
    let str = UInt8.to_string opcode in
    let folder = "../data/opcode/0x" ^ String.make 1 str.[3] ^ "/0x" in
    let file = folder ^ String.make 1 str.[3] ^ String.make 1 str.[4] ^ ".json" in
    Yojson.Basic.from_file file

(** [check_ram ram1 ram2 info] checks to see if the address value pairs described
    in [info] exist in both [ram1] and [ram2]. *)
let check_ram (ram1 : Ram.t) (ram2 : Ram.t) (info : rinfo) : bool =
    let check (ram : Ram.t) ((addr, value) : int * int) =
        Ram.read_ui8 ram ~..addr <-> ~.value
    in
    List.for_all (check ram1) info && List.for_all (check ram2) info

(** [compare_cpu cpu1 cpu2 info] compares the given [cpu1] and [cpu2] with the
    given [info]. *)
let compare_cpu (info : rinfo) (cpu1 : Cpu.t) (cpu2 : Cpu.t) : bool =
    cpu1.accumulator <-> cpu2.accumulator
    && cpu1.registerX <-> cpu2.registerX
    && cpu1.registerY <-> cpu2.registerY
    && cpu1.progCounter <--> cpu2.progCounter
    && cpu1.stackPointer <-> cpu2.stackPointer
    && Cpu.flags_to_ui8 cpu1.flags <-> Cpu.flags_to_ui8 cpu2.flags
    && check_ram cpu1.ram cpu2.ram info

(** [ram_to_string info cpu] converts the given [info] and [cpu] into a ram string. *)
let ram_to_string (info : rinfo) (cpu : Cpu.t) : string =
    Printf.sprintf "RAM: [ %s ]\n"
        (String.concat ", "
             (List.map
                  (fun (addr, value) ->
                       Printf.sprintf "(%s, %s)" (UInt16.to_string ~..addr)
                           (UInt8.to_string (Ram.read_ui8 cpu.ram ~..addr)))
                  info))

(** [cpu_to_string info cpu] converts the given [info] and [cpu] into a cpu string. *)
let cpu_to_string (info : rinfo) (cpu : Cpu.t) : string =
    Printf.sprintf "Cpu: { PC: %s, SP: %s, A: %s, X: %s, Y: %s, F: %s }\n"
        (UInt16.to_string cpu.progCounter)
        (UInt8.to_string cpu.stackPointer)
        (UInt8.to_string cpu.accumulator)
        (UInt8.to_string cpu.registerX)
        (UInt8.to_string cpu.registerY)
        (UInt8.to_string (Cpu.flags_to_ui8 cpu.flags))
    ^ ram_to_string info cpu

(** [make_opcode_test test opcode] confirms the given opcode steps properly. *)
let make_opcode_test (test : opcode_test) (opcode : uint8) : test =
    let cpu_step = Opcode.step test.initial_state opcode in
    test.name >:: fun _ ->
        assert_equal test.final_state cpu_step
            ~cmp:(compare_cpu test.final_ram)
            ~printer:(cpu_to_string test.final_ram)

(** Opcode tests to be run. *)
let tests : test list =
    let parse (i : int) : json = parse_json ~.i in
    let json_list : json list = List.init 256 parse in
    let opcode_tests : opcode_test list list = List.map from_json json_list in
    let mapi =
        List.mapi (fun i l -> List.map (fun t -> make_opcode_test t ~.i) l)
    in
    let tests : test list list =
        List.filteri (fun i j -> i <> 0x6B) (mapi opcode_tests)
    in
    List.flatten tests
