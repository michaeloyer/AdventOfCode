type Direction = Up of int | Down of int | Forward of int

module Data =
    open System.IO
    let private readData name =
        let (|Line|_|) = function
            | [| direction; amount |] -> Some(direction, amount |> int)
            | _ -> None

        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadLines
        |> Seq.map (fun line ->
            match line.Split(" ") with
            | Line("up", i) -> Up i
            | Line("down", i) -> Down i
            | Line("forward", i) -> Forward i
            | otherwise -> failwithf "Parsing Failed: %A" otherwise
        )

    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
    type Position = {
        X: int
        Y: int
        Aim: int
    }
    let startPosition = { X = 0; Y = 0; Aim = 0 }

    let multiplyPosition { X = x; Y = y } = x * y
    let part1 data =
        (startPosition, data)
        ||> Seq.fold (fun position ->
            function
            | Forward i -> { position with X = position.X + i }
            | Up i -> { position with Y = position.Y - i }
            | Down i -> { position with Y = position.Y + i }
        )
        |> multiplyPosition

    let part2 data =
        (startPosition, data)
        ||> Seq.fold (fun position ->
            function
            | Down i -> { position with Aim = position.Aim + i }
            | Up i -> { position with Aim = position.Aim - i }
            | Forward i ->
                { position with
                    X = position.X + i
                    Y = position.Y + (position.Aim * i) }
        )
        |> multiplyPosition

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

