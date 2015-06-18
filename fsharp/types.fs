[<AutoOpen>]
module Types

open System

type Token =
    | Nil
    | Boolean of bool
    | Integer of Int32
    | FloatingPoint of float
    | Symbol of string
    | Keyword of string
    | String of string
    | List of Token list
    | Vector of Token list
    | Hash of Map<Token, Token>
    | Quote of Token list
    | QuasiQuote of Token list
    | Unquote of Token list
    | SpliceUnquote of Token list
    | Dereference of Token list
    | WithMeta of Token list * Token

exception UnbalancedParenthesesError of string * bool
exception UnmatchedParenthesesError of string
exception HashMapKeyWithoutValueError of string * string
exception StringLiteralNotClosedError of string
exception EmptyInputError
