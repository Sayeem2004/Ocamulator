open Alias

type t = {
    size : int;
    memory : bytes;
}

let zero_ram () : t =
    { size = 0xFFFF + 1; memory = Bytes.make (0xFFFF + 1) '\x00' }

let nes_ram (rom : bytes) =
    let index i = if i < Bytes.length rom then Bytes.get rom i else '\x00' in
    { size = 0xFFFF + 1; memory = Bytes.init (0xFFFF + 1) index }

let read_ui8 (ram : t) (addr : uint16) : uint8 =
    Bytes.get_uint8 ram.memory ~**addr |> ( ~. )

let write_ui8 (ram : t) (addr : uint16) (value : uint8) : unit =
    Bytes.set_uint8 ram.memory ~**addr ~*value
