type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    open type System.StringSplitOptions

    let parseGroups (text:string) =
        text.Split("\n\n", RemoveEmptyEntries)
        |> Array.map (fun group ->
            group.Split("\n", RemoveEmptyEntries) |> Seq.map int |> Seq.sum
        )

    let part1 (puzzle:PuzzleInput) =
        puzzle.FullText
        |> parseGroups
        |> Array.max

    let part2 (puzzle:PuzzleInput) =
        puzzle.FullText
        |> parseGroups
        |> Seq.sortDescending |> Seq.take 3 |> Seq.sum

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
