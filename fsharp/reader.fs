module Reader

open System
open System.Text.RegularExpressions

let private tokenize =
    let pattern = @"[\s,]*(~@|[\[\]{}()'`~^@]|""(?:\\.|[^\\""])*""|;.*|[^\s\[\]{}('""`,;)]*)"
    let comment = Regex(@";.*$", RegexOptions.Compiled)
    let token = Regex(pattern, RegexOptions.Compiled)
    fun str ->
        let str = comment.Replace(str, "")
        if String.IsNullOrWhiteSpace str then raise EmptyInputError else
        [for m in token.Matches(str) do let mg = m.Groups.[1].Value in if mg.Length > 0 then yield mg]

let read_str = tokenize >> Ast.read_from
