#r "nuget:FParsec"

type Pipe = {
    X1: int
    Y1: int
    X2: int
    Y2: int
}

module Data =
    open FParsec
    open System.IO

    module Parsers =
        let range = pint32 .>> skipChar ',' .>>. pint32
        let pipe =
            range .>> skipString " -> " .>>. range
            |>> fun ((x1, y1),(x2, y2)) ->
                { X1 = x1; Y1 = y1; X2 = x2; Y2 = y2 }
        let pipes = sepEndBy pipe newline

    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadAllText
        |> fun text ->
            match run Parsers.pipes text with
            | Success (pipes, _, _) -> pipes
            | Failure (error, _, _) -> failwith error
    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
    let sumOverlappingCoordinates coordinates =
        coordinates
        |> List.groupBy id
        |> List.filter (fun (_, group) -> List.length group > 1)
        |> List.map (snd >> List.head)
        |> List.length

    let part1 data =
        [ for {X1 = x1; X2 = x2; Y1 = y1; Y2 = y2} in data do
            if x1 = x2 then
                let x = x1
                for y in (min y1 y2) .. (max y1 y2) do
                    yield (x, y)
            elif y1 = y2 then
                let y = y1
                for x in (min x1 x2) .. (max x1 x2) do
                    yield (x, y)
        ]
        |> sumOverlappingCoordinates

    let part2 data =
        [ for {X1 = x1; X2 = x2; Y1 = y1; Y2 = y2} in data do
            if x1 = x2 then
                let x = x1
                for y in (min y1 y2) .. (max y1 y2) do
                    yield (x, y)
            elif y1 = y2 then
                let y = y1
                for x in (min x1 x2) .. (max x1 x2) do
                    yield (x, y)
            elif (abs (x1 - x2) = abs (y1 - y2)) then
                let range i j = if i < j then [for a = i to j do a] else [for a = i downto j do a]
                yield! List.zip (range x1 x2) (range y1 y2)
        ]
        |> sumOverlappingCoordinates

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
