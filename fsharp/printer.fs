module Printer

open System

let rec private pr_str_ = function
    | Integer(i) -> sprintf "%d" i
    | Symbol(s) -> sprintf "%s" s
    | List(tokens) ->
        sprintf "(%s)" <| String.Join(" ", List.map pr_str_ tokens)
    | Vector(tokens) ->
        sprintf "[%s]" <| String.Join(" ", List.map pr_str_ tokens)
    | Quote(tokens) ->
        sprintf "(quote %s)" <| String.Join("", List.map pr_str_ tokens)
    | QuasiQuote(tokens) ->
        sprintf "(quasiquote %s)" <| String.Join("", List.map pr_str_ tokens)
    | Unquote(tokens) ->
        sprintf "(unquote %s)" <| String.Join("", List.map pr_str_ tokens)
    | SpliceUnquote(tokens) ->
        sprintf "(splice-unquote %s)" <| String.Join("", List.map pr_str_ tokens)

let pr_str tokens = String.Join(" ", List.map pr_str_ tokens)
