module Data =
    open System.IO
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadAllText
        |> fun text -> text.Split(',')
        |> Seq.map int
    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
    let rec countDown : (bigint * bigint * bigint * bigint * bigint * bigint * bigint * bigint * bigint) * int -> bigint = function
        | (f0,f1,f2,f3,f4,f5,f6,f7,f8), 0 ->
            [f0;f1;f2;f3;f4;f5;f6;f7;f8]
            |> List.reduce (+)
        | (f0,f1,f2,f3,f4,f5,f6,f7,f8),timer ->
            countDown ((f1,f2,f3,f4,f5,f6,f7+f0,f8,f0), (timer - 1))

    let groupNumbers numbers =
        numbers
        |> Seq.groupBy id
        |> Seq.map (fun (key, numbers) -> key, numbers |> Seq.length |> bigint)
        |> Map.ofSeq
        |> fun map ->
            let find key = map |> Map.tryFind key |> Option.defaultValue 0I
            (find 0, find 1, find 2, find 3, find 4, find 5, find 6, find 7, find 8)

    let part1 data = (data |> groupNumbers, 80) |> countDown

    let part2 data = (data |> groupNumbers, 256) |> countDown

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

