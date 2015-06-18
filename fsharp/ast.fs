module Ast

module private Parentheses =
    let private openingOnes = "([{<"
    let private closingOnes = ")]}>"

    let private inOne (haystack:string) (needle:string) =
        if haystack.IndexOf needle >= 0 then Some needle else None

    let (|Opening|_|) = inOne openingOnes
    let (|Closing|_|) = inOne closingOnes

    let (|Round|Square|Curly|Angle|None|) = function
        | "(" -> Round ")"
        | "[" -> Square "]"
        | "{" -> Curly "}"
        | "<" -> Angle ">"
        | _ -> None

let rec private make_hash acc = function
    | [] -> Hash(Map.ofList acc)
    | [kw] ->
        let printed = Printer.pr_str_single false kw
        HashMapKeyWithoutValueError(sprintf "Hash keyword '%s' missing its value." printed, printed) |> raise
    | key::value::rest ->
        make_hash ((key, value)::acc) rest

let rec private read acc closing tokens =
    match tokens, closing with
    | [], None -> List.rev acc, []
    | [], Some closing ->
        let message = sprintf "Unexpected end of input, still waiting for %s." closing
        UnbalancedParenthesesError(message, true) |> raise
    | stop::rest, Some closing when stop = closing -> List.rev acc, rest
    | Parentheses.Closing stop::rest, Some closing ->
        let message =
            sprintf "Wrong kind of closing parenthesis encountered, expected %s but got %s." closing stop
        UnmatchedParenthesesError(message) |> raise
    | Parentheses.Closing stop::rest, None ->
        let message = sprintf "Unexpected %s encountered." stop
        UnbalancedParenthesesError(message, false) |> raise
    | Parentheses.Opening opening::rest, _ ->
        let inner, rest = read_list opening rest
        read (inner::acc) closing rest
    | "'"::rest, _ ->
        let quoted, rest = read_quote rest
        read (quoted::acc) closing rest
    | "`"::rest, _ ->
        let quoted, rest = read_quasiquote rest
        read (quoted::acc) closing rest
    | "~"::rest, _ ->
        let unquoted, rest = read_unquote rest
        read (unquoted::acc) closing rest
    | "~@"::rest, _ ->
        let unquoted, rest = read_splice_unquote rest
        read (unquoted::acc) closing rest
    | "@"::rest, _ ->
        let deref, rest = read_dereference rest
        read (deref::acc) closing rest
    | atom::rest, _ ->
        read (Atom.read_atom atom::acc) closing rest

and private read_list which tokens =
    match which with
    | Parentheses.Round closing ->
        let inner, rest = read [] (Some closing) tokens
        List(inner), rest
    | Parentheses.Square closing ->
        let inner, rest = read [] (Some closing) tokens
        Vector(inner), rest
    | Parentheses.Curly closing ->
        let inner, rest = read [] (Some closing) tokens
        make_hash [] inner, rest
    | _ -> failwith "Unknown list type."

and private read_quote tokens =
    let inner, rest = read [] None tokens
    Quote(inner), rest

and private read_quasiquote tokens =
    let inner, rest = read [] None tokens
    QuasiQuote(inner), rest

and private read_unquote tokens =
    let inner, rest = read [] None tokens
    Unquote(inner), rest

and private read_splice_unquote tokens =
    let inner, rest = read [] None tokens
    SpliceUnquote(inner), rest

and private read_dereference tokens =
    let inner, rest = read [] None tokens
    Dereference inner, rest

let read_from strl =
    match read [] None strl with
    | re, [] -> re
    | _ -> failwith "Ast.read_from: Should never get here."

