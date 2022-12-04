type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    open System
    let letterPriority letter =
        if Char.IsLower letter then
            int letter - int 'a' + 1
        else
            int letter - int 'A' + 27

    let part1 (puzzle:PuzzleInput) =
        puzzle.Lines
        |> Seq.sumBy (fun line ->
            let rucksack = Array.splitInto 2 (line.ToCharArray())
            Set.intersect (Set(rucksack[0])) (Set(rucksack[1]))
            |> Seq.head
            |> letterPriority
        )

    let part2 (puzzle:PuzzleInput) =
        puzzle.Lines
        |> Seq.map Set
        |> Seq.chunkBySize 3
        |> Seq.sumBy (Set.intersectMany >> Seq.head >> letterPriority)

module Data =
    open System.IO
    let private readData name =
        let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt"))
        { Lines = List.ofArray lines
          FullText = String.concat "\n" lines }

    let rec example = readData (nameof example)
    let rec puzzle = readData (nameof puzzle)

module Answers =
    fsi.AddPrintTransformer(fun (input:PuzzleInput) -> {| LineCount = input.Lines.Length |})
    let rec ``Example Part 1`` = let answer = AdventOfCode.part1 Data.example in printfn $"{nameof ``Example Part 1``}: {answer}"; answer
    let rec ``Puzzle Part 1`` = let answer = AdventOfCode.part1 Data.puzzle in printfn $"{nameof ``Puzzle Part 1``}: {answer}"; answer
    let rec ``Example Part 2`` = let answer = AdventOfCode.part2 Data.example in printfn $"{nameof ``Example Part 2``}: {answer}"; answer
    let rec ``Puzzle Part 2`` = let answer = AdventOfCode.part2 Data.puzzle in printfn $"{nameof ``Puzzle Part 2``}: {answer}"; answer
