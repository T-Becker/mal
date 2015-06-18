module Printer

open System
open System.Globalization
open System.Text.RegularExpressions

module Str = Microsoft.FSharp.Core.String

let private convert_str =
    let newline = Regex(@"(?<!\\)\\n", RegexOptions.Compiled)
    let quotes = Regex(@"(?<!\\)\\""", RegexOptions.Compiled)
    fun str ->
        quotes.Replace(newline.Replace(str, "\n"), "\"").Replace(@"\\", @"\")

let private concat : string seq -> string = Str.concat " "

let rec pr_str_single print_readably str =
    let me = pr_str_single print_readably
    let flatten format = List.map me >> concat >> sprintf format
    match str with
    | Nil -> "nil"
    | Boolean b -> if b then "true" else "false"
    | FloatingPoint f -> f.ToString CultureInfo.InvariantCulture
    | Integer i -> sprintf "%d" i
    | Symbol s -> sprintf "%s" s
    | Keyword k -> sprintf ":%s" k
    | String str -> sprintf "\"%s\"" <| if print_readably then str else convert_str str
    | List tokens -> tokens |> flatten "(%s)"
    | Vector tokens -> tokens |> flatten "[%s]"
    | Hash hash ->
        hash
        |> Map.toSeq
        |> Seq.collect (fun (k, v) -> [me k; me v])
        |> concat
        |> sprintf "{%s}"
    | Quote tokens -> tokens |> flatten "(quote %s)"
    | QuasiQuote tokens -> tokens |> flatten "(quasiquote %s)"
    | Unquote tokens -> tokens |> flatten "(unquote %s)"
    | SpliceUnquote tokens -> tokens |> flatten "(splice-unquote %s)"
    | Dereference tokens -> tokens |> flatten "(deref %s)"
    | WithMeta(tokens, meta) ->
        sprintf "(with-meta %s %s)" (tokens |> List.map me |> concat) (me meta)

let pr_str print_readably tokens =
    [
        tokens |> List.map (pr_str_single print_readably) |> concat
        "\n"
    ] |> Str.concat ""
