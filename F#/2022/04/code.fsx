type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    let parseRanges (text:string) =
        let numbers = text.Split(',', '-')
        (Set [ (int numbers[0])..(int numbers[1])],
         Set [ (int numbers[2])..(int numbers[3])])

    let part1 (puzzle:PuzzleInput) =
        puzzle.Lines
        |> Seq.map parseRanges
        |> Seq.filter (fun (set1, set2) ->
            Set.isSubset set1 set2 || Set.isSuperset set1 set2
        )
        |> Seq.length

    let part2 (puzzle:PuzzleInput) =
        puzzle.Lines
        |> Seq.map parseRanges
        |> Seq.filter (fun (set1, set2) ->
            Set.exists (fun item -> Set.contains item set2) set1
        )
        |> Seq.length

module Input =
    open System.IO
    let private readData name =
        let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt"))
        { Lines = List.ofArray lines
          FullText = String.concat "\n" lines }

    let rec example = readData (nameof example)
    let rec puzzle = readData (nameof puzzle)

module Output =
    fsi.AddPrintTransformer(fun (input:PuzzleInput) -> {| LineCount = input.Lines.Length |})
    let rec ``Example Part 1`` = let answer = AdventOfCode.part1 Input.example in printfn $"{nameof ``Example Part 1``}: {answer}"; answer
    let rec ``Puzzle Part 1`` = let answer = AdventOfCode.part1 Input.puzzle in printfn $"{nameof ``Puzzle Part 1``}: {answer}"; answer
    let rec ``Example Part 2`` = let answer = AdventOfCode.part2 Input.example in printfn $"{nameof ``Example Part 2``}: {answer}"; answer
    let rec ``Puzzle Part 2`` = let answer = AdventOfCode.part2 Input.puzzle in printfn $"{nameof ``Puzzle Part 2``}: {answer}"; answer
