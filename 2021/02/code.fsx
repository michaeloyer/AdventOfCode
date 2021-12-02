#r "nuget: FParsec"

type Direction = Up of int | Down of int | Forward of int

module Data =
    open FParsec
    open System.IO

    module Parsers =
        let up = pstring "up " >>. pint32 |>> Up
        let down = pstring "down " >>. pint32 |>> Down
        let forward = pstring "forward " >>. pint32 |>> Forward

        let direction = choice [ up; down; forward ]

    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadAllText
        |> run (sepEndBy Parsers.direction newline)
        |> function
            | Success (directions, _, _) -> directions
            | Failure (message, _, _) -> failwith message

    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
    let part1 data =
        ((0,0), data)
        ||> Seq.fold (fun (x, y) ->
            function
            | Forward i -> (x + i, y)
            | Up i -> (x, y - i)
            | Down i -> (x, y + i)
        )
        |> (fun (x, y) -> x * y)

    type Position = {
        X: int
        Y: int
        Aim: int
    } with static member Empty = { X = 0; Y = 0; Aim = 0 }

    let part2 data =
        (Position.Empty, data)
        ||> Seq.fold (fun position ->
            function
            | Down i -> { position with Aim = position.Aim + i }
            | Up i -> { position with Aim = position.Aim - i }
            | Forward i ->
                { position with
                    X = position.X + i
                    Y = position.Y + (position.Aim * i) }
        )
        |> fun { X = x; Y = y } -> x * y

module Answers =
    open Data
    open Code

    let ``Example Part 1`` = part1 example
    let ``Puzzle Part 1`` = part1 puzzle
    let ``Example Part 2`` = part2 example
    let ``Puzzle Part 2`` = part2 puzzle

    do
        printfn $"{nameof(``Example Part 1``)}: {``Example Part 1``}"
        printfn $"{nameof(``Puzzle Part 1``)}: {``Puzzle Part 1``}"
        printfn $"{nameof(``Example Part 2``)}: {``Example Part 2``}"
        printfn $"{nameof(``Puzzle Part 2``)}: {``Puzzle Part 2``}"

