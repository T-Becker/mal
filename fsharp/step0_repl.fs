open System

let READ str = str

let EVAL str = str

let PRINT str = str

let rep = READ >> EVAL >> PRINT

let rec main () =
    printf "user> "
    let line = Console.ReadLine()
    if line <> null then
        line |> rep |> printfn "%s"
        main()

main()