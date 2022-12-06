type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    let findMarker charCount (line:string)  =
            line
            |> Seq.windowed charCount
            |> Seq.mapi (fun i chars ->
                i + charCount, Set chars
            )
            |> Seq.find (fun (_, set) -> Set.count set = charCount)
            |> fst
    let part1 (puzzle:PuzzleInput) =
        List.map (findMarker 4) puzzle.Lines

    let part2 (puzzle:PuzzleInput) =
        List.map (findMarker 14) puzzle.Lines

module Input =
    open System.IO
    let private readData name =
        let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt"))
        { Lines = List.ofArray lines
          FullText = String.concat "\n" lines }

    let rec example = readData (nameof example)
    let rec puzzle = readData (nameof puzzle)

module Output =
    type PuzzleInputOutput = { LineCount: int }
    fsi.AddPrintTransformer(fun (input:PuzzleInput) -> { LineCount = input.Lines.Length })
    let rec ``Example Part 1`` = let answer = AdventOfCode.part1 Input.example in printfn $"{nameof ``Example Part 1``}: {answer}"; answer
    let rec ``Puzzle Part 1`` = let answer = AdventOfCode.part1 Input.puzzle in printfn $"{nameof ``Puzzle Part 1``}: {answer}"; answer
    let rec ``Example Part 2`` = let answer = AdventOfCode.part2 Input.example in printfn $"{nameof ``Example Part 2``}: {answer}"; answer
    let rec ``Puzzle Part 2`` = let answer = AdventOfCode.part2 Input.puzzle in printfn $"{nameof ``Puzzle Part 2``}: {answer}"; answer
