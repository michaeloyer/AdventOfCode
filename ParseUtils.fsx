[<AutoOpen>]
module ParseUtils

#r "nuget: FParsec"

open FParsec
let parseText parser text : 'T =
    run parser text
    |> function
        | Success (value, _, _) -> value
        | Failure (message, _, _) -> failwith message
