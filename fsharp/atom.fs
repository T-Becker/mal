module Atom

open System
open System.Globalization

let (|ParsableBoolean|_|) str =
    match bool.TryParse str with
    | true, parsed -> Some parsed
    | _ -> None

let (|ParsableInt|_|) str =
    match Int32.TryParse str with
    | true, parsed -> Some parsed
    | _ -> None

let (|ParsableFloat|_|) str =
    match Double.TryParse(str, Globalization.NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, parsed -> Some parsed
    | _ -> None

let (|KeywordString|_|) (str:string) =
    if not <| str.StartsWith(":") then None else
    Some str.[1..]

let (|QuotedString|_|) (str:string) =
    if not <| str.StartsWith("\"") then None else
    Some str.[1..str.Length - 2]

let read_atom token =
    match token with
    | "nil" -> Token.Nil
    | KeywordString kw -> Keyword kw
    | QuotedString str -> Token.String str
    | ParsableBoolean aBool -> Token.Boolean aBool
    | ParsableInt anInt -> Integer anInt
    | ParsableFloat aFloat -> FloatingPoint aFloat
    | _ -> Symbol token
