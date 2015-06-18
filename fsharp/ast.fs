module Ast

module private Parentheses =
    let private openingOnes = "([{"
    let private closingOnes = ")]}"

    let private isOne (which:string) (what:string) = which.IndexOf(what) >= 0
    let opening = isOne openingOnes
    let closing = isOne closingOnes

    let matching =
        Seq.zip openingOnes closingOnes
        |> Seq.collect (fun (a, b) -> [(string a, string b); (string b, string a)])
        |> Map.ofSeq

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
    | stop::rest, Some closing when Parentheses.closing stop ->
        let message =
            sprintf "Wrong kind of closing parenthesis encountered, expected %s but got %s." closing stop
        UnmatchedParenthesesError(message) |> raise
    | stop::rest, None when Parentheses.closing stop ->
        let message = sprintf "Unexpected %s encountered." stop
        UnbalancedParenthesesError(message, false) |> raise
    | opening::rest, _ when Parentheses.opening opening ->
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
    | atom::rest, _ ->
        read (Atom.read_atom atom::acc) closing rest

and private read_list which tokens =
    let inner, rest = read [] (Some Parentheses.matching.[which]) tokens
    match which with
    | "(" -> List(inner), rest
    | "[" -> Vector(inner), rest
    | "{" -> make_hash [] inner, rest
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

let read_from strl =
    match read [] None strl with
    | re, [] -> re
    | _ -> failwith "Ast.read_from: Should never get here."

