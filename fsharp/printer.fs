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

let rec pr_str_single print_readably = function
    | Nil -> "nil"
    | Boolean b -> if b then "true" else "false"
    | FloatingPoint f -> f.ToString CultureInfo.InvariantCulture
    | Integer i -> sprintf "%d" i
    | Symbol s -> sprintf "%s" s
    | Keyword k -> sprintf ":%s" k
    | String str -> sprintf "\"%s\"" <| if print_readably then convert_str str else str
    | List tokens ->
        sprintf "(%s)" <| String.Join(" ", tokens |> List.map (pr_str_single print_readably))
    | Vector tokens ->
        sprintf "[%s]" <| String.Join(" ", tokens |> List.map (pr_str_single print_readably))
    | Hash h ->
        sprintf "{%s}"
        <| String.Join(" ", h |> Map.toSeq |> Seq.collect (fun (k, v) ->
            [pr_str_single print_readably k; pr_str_single print_readably v]))
    | Quote tokens ->
        sprintf "(quote %s)" <| String.Join("", tokens |> List.map (pr_str_single print_readably))
    | QuasiQuote tokens ->
        sprintf "(quasiquote %s)" <| String.Join("", tokens |> List.map (pr_str_single print_readably))
    | Unquote tokens ->
        sprintf "(unquote %s)" <| String.Join("", tokens |> List.map (pr_str_single print_readably))
    | SpliceUnquote tokens ->
        sprintf "(splice-unquote %s)" <| String.Join("", tokens |> List.map (pr_str_single print_readably))

let pr_str print_readably tokens =
    (tokens
    |> List.map (pr_str_single print_readably)
    |> Str.concat " "
    ) + "\n"
