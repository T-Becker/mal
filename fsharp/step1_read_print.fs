#if INTERACTIVE
#r "mal.dll"
#endif

open System

let READ = Reader.read_str

let EVAL str = str

let PRINT = Printer.pr_str

let rep = READ >> EVAL >> PRINT

let rec main () =
    let printError = printfn "error> %s"
    printf "user> "
    let line = Console.ReadLine()
    if line <> null then
        try
            line |> rep |> printfn "%s"
        with 
        | UnbalancedParenthesesError(msg, _) ->
            printError msg
        | UnmatchedParenthesesError msg ->
            printError msg
        main()

main()