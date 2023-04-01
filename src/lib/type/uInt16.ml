open UInt8

module UInt16Set : UIntSet.S with type t := int = struct
    let (max_value : int) = 0xFFFF
    let (zero : int) = 0
    let (one : int) = 1
    let (size : int) = 16
end

module UInt16 = struct
    include UIntX.F (UInt16Set)

    let ui16_from_ui8 (ui8 : uint8) : t = from_int (UInt8.to_int ui8)

    let ui16_combine_ui8 (most_sig_bit : uint8) (least_sig_bit : uint8) : t =
        shift_left (ui16_from_ui8 most_sig_bit) 8
        |> add (ui16_from_ui8 least_sig_bit)
end

type uint16 = UInt16.t

let ( <??> ) = UInt16.compare
let ( <--> ) = UInt16.equal
let ( +++ ) = UInt16.add
let ( !++ ) = UInt16.succ
let ( --- ) = UInt16.sub
let ( !-- ) = UInt16.pred
let ( *** ) = UInt16.mul
let ( /// ) = UInt16.div
let ( %%% ) = UInt16.rem
let ( &&& ) = UInt16.logand
let ( |/| ) = UInt16.logxor
let ( ||| ) = UInt16.logor
let ( <<< ) = UInt16.shift_left
let ( >>> ) = UInt16.shift_right
let ( ~^ ) = UInt16.from_int
let ( ?^ ) i = Bool.to_int i |> UInt16.from_int
let ( !^ ) = UInt16.ui16_from_ui8
let ( !. ) i = UInt16.to_int i |> UInt8.from_int