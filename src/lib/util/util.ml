open Alias
open Graphics

let open_rom (name : string) : in_channel =
    In_channel.open_bin ("../data/rom/" ^ name)

let set_color (u : uint8) : unit =
    match ~*u with
    | 0x0 -> Graphics.set_color (Graphics.rgb 000 000 000)
    | 0x1 -> Graphics.set_color (Graphics.rgb 255 255 255)
    | 0x2 -> Graphics.set_color (Graphics.rgb 255 000 000)
    | 0x3 -> Graphics.set_color (Graphics.rgb 000 255 255)
    | 0x4 -> Graphics.set_color (Graphics.rgb 128 000 128)
    | 0x5 -> Graphics.set_color (Graphics.rgb 000 255 000)
    | 0x6 -> Graphics.set_color (Graphics.rgb 000 000 255)
    | 0x7 -> Graphics.set_color (Graphics.rgb 255 255 000)
    | 0x8 -> Graphics.set_color (Graphics.rgb 255 165 000)
    | 0x9 -> Graphics.set_color (Graphics.rgb 150 075 000)
    | 0xA -> Graphics.set_color (Graphics.rgb 255 069 000)
    | 0xB -> Graphics.set_color (Graphics.rgb 105 105 105)
    | 0xC -> Graphics.set_color (Graphics.rgb 169 169 169)
    | 0xD -> Graphics.set_color (Graphics.rgb 144 238 144)
    | 0xE -> Graphics.set_color (Graphics.rgb 173 216 230)
    | 0xF -> Graphics.set_color (Graphics.rgb 211 211 211)
    | _ -> Graphics.set_color (Graphics.rgb 255 000 255)

let psize : int = 24
let ssize : int = 32
let stall : int = 1200
