module Data =
    open System.IO
    open type System.StringSplitOptions
    
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadAllText
        |> fun text -> text.Split("\n\n", RemoveEmptyEntries)
        |> Array.map (fun group -> group.Split("\n", RemoveEmptyEntries) |> Seq.map int |> Seq.sum)

    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
    let part1 (data:int array) =
        Array.max data


    let part2 data =
        data |> Seq.sortDescending |> Seq.take 3 |> Seq.sum

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

