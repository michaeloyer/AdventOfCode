open System

module Data =
    open System.IO
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadLines
        |> Seq.filter (fun line ->
            not (String.IsNullOrWhiteSpace line)
        )
    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
    let letterPriority letter =
        if Char.IsLower letter then
            int letter - int 'a' + 1
        else
            int letter - int 'A' + 27

    let part1 (data: string seq) =
        data
        |> Seq.sumBy (fun line ->
            let rucksack = Array.splitInto 2 (line.ToCharArray())
            Set.intersect (Set(rucksack[0])) (Set(rucksack[1]))
            |> Seq.head
            |> letterPriority
        )

    let part2 (data: string seq) =
        data
        |> Seq.map Set
        |> Seq.chunkBySize 3
        |> Seq.sumBy (Set.intersectMany >> Seq.head >> letterPriority)

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
