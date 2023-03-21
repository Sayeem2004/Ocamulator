open UInt8
open UInt16


module UIntVar = struct
    type uint_var =
    | U8 of uint8
    | U16 of uint16
    | Temp8 of int
    | Temp16 of int

    exception UnknownOperation of string

    let to_int (v : uint_var) : int = match v with
    | U8 a -> UInt8.to_int a
    | U16 a -> UInt16.to_int a
    | Temp8 a | Temp16 a -> a

    let rec ( ++ ) (v : uint_var) (u : uint_var) : uint_var = match (v ,u) with
    | (U8  a, U8  b) -> U8 (UInt8.add a b)
    | (U8  a, U16 b) -> U16 (UInt16.add (UInt16.ui16_from_ui8 a) b)
    | (U16 a, U8  b) -> U16 (UInt16.add (UInt16.ui16_from_ui8 b) a)
    | (U16 a, U16 b) -> U16 (UInt16.add a b)
    | (Temp8  a, _)  -> U8(UInt8.from_int a) ++ u
    | (Temp16 a, _)  -> U16(UInt16.from_int a) ++ u
    | (_, Temp8  b)  -> U8(UInt8.from_int b) ++ v
    | (_, Temp16 b)  -> U16(UInt16.from_int b) ++ v
    
    let rec ( -- ) (v : uint_var) (u : uint_var) : uint_var = match (v ,u) with
    | (U8  a, U8  b) -> U8  (UInt8.sub a b)
    | (U8  a, U16 b) -> U16 (UInt16.sub (UInt16.ui16_from_ui8 a) b)
    | (U16 a, U8  b) -> U16 (UInt16.sub (UInt16.ui16_from_ui8 b) a)
    | (U16 a, U16 b) -> U16 (UInt16.sub a b)
    | (Temp8  a, _)  -> U8  (UInt8.from_int a)  -- u
    | (Temp16 a, _)  -> U16 (UInt16.from_int a) -- u
    | (_, Temp8  b)  -> U8  (UInt8.from_int b)  -- v
    | (_, Temp16 b)  -> U16 (UInt16.from_int b) -- v
    
    let rec ( && ) (v : uint_var) (u : uint_var) : uint_var = match (v ,u) with
    | (U8  a, U8  b) -> U8  (UInt8.logand a b)
    | (U8  a, U16 b) -> U16 (UInt16.logand (UInt16.ui16_from_ui8 a) b)
    | (U16 a, U8  b) -> U16 (UInt16.logand (UInt16.ui16_from_ui8 b) a)
    | (U16 a, U16 b) -> U16 (UInt16.logand a b)
    | (Temp8  a, _)  -> U8  (UInt8.from_int a)  && u
    | (Temp16 a, _)  -> U16 (UInt16.from_int a) && u
    | (_, Temp8  b)  -> U8  (UInt8.from_int b)  && v
    | (_, Temp16 b)  -> U16 (UInt16.from_int b) && v
end


