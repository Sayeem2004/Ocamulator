type uint4 = UInt4.t
type uint8 = UInt8.t
type uint16 = UInt16.t

let ( <?> ) = UInt8.compare
let ( <-> ) = UInt8.equal
let ( ++ ) = UInt8.add
let ( -- ) = UInt8.sub
let ( &&. ) = UInt8.logand
let ( |&. ) = UInt8.logxor
let ( ||. ) = UInt8.logor
let ( << ) = UInt8.left
let ( >> ) = UInt8.right
let ( ~. ) = UInt8.from_int
let ( ~* ) = UInt8.to_int
let ( ?. ) b = Bool.to_int b |> UInt8.from_int
let ( ?* ) u = u <?> UInt8.zero = 0
let ( ?- ) u = not (u &&. ~.0x80 <-> ~.0x00)
let ( ?+ ) u = not (u &&. ~.0x01 <-> ~.0x00)
let ( ?@ ) u = u |&. ~.0xFF
let ( ?% ) u = if ?-u then -(256 - ~*u) else ~*u
let ( <??> ) = UInt16.compare
let ( <--> ) = UInt16.equal
let ( +++ ) = UInt16.add
let ( --- ) = UInt16.sub
let ( &&& ) = UInt16.logand
let ( <<< ) = UInt16.left
let ( >>> ) = UInt16.right
let ( ~.. ) = UInt16.from_int
let ( ~** ) = UInt16.to_int
let ( !.. ) = UInt16.combine_ui8
let ( !** ) = UInt16.ui16_from_ui8
let ( !-- ) u = UInt16.to_int u |> UInt8.from_int
let ( !++ ) b = Bool.to_int b |> UInt16.from_int
let ( !@@ ) = UInt16.split_ui16

let nth_bit_ui8 (ui8 : uint8) (n : int) = not (ui8 &&. ~.(0b00000001 lsl n) <-> ~.0x00)
