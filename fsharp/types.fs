[<AutoOpen>]
module Types

open System

type Token =
    | Integer of Int32
    | Symbol of string
    | List of Token list
    | Vector of Token list
    | Quote of Token list
    | QuasiQuote of Token list
    | Unquote of Token list
    | SpliceUnquote of Token list

exception UnbalancedParenthesesError of string * bool
exception UnmatchedParenthesesError of string
