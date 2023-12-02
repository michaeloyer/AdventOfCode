type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    open System

    let part1 (puzzle:PuzzleInput) =
        let readLineCalibration line =
            let array = [| for char in line do if Char.IsDigit char then yield char |]
            int $"{Array.head array}{Array.last array}"

        List.sumBy readLineCalibration puzzle.Lines

    open System.Text.RegularExpressions
    let part2 (puzzle:PuzzleInput) =
        let getNumeric text =
            match text with
            | "one" -> 1
            | "two" -> 2
            | "three" -> 3
            | "four" -> 4
            | "five" -> 5
            | "six" -> 6
            | "seven" -> 7
            | "eight" -> 8
            | "nine" -> 9
            | value -> int value

        let readLineCalibration (line:string) =
            let pattern = @"(\d|one|two|three|four|five|six|seven|eight|nine)"
            let left = Regex.Match(line, pattern).Value |> getNumeric
            let right = Regex.Match(line, pattern, RegexOptions.RightToLeft).Value |> getNumeric
            left * 10 + right

        List.sumBy readLineCalibration puzzle.Lines

module Input =
    open System.IO
    let private readData name =
        let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt"))
        { Lines = List.ofArray lines
          FullText = String.concat "\n" lines }

    let rec example = readData (nameof example)
    let rec example2 = readData (nameof example2)
    let rec puzzle = readData (nameof puzzle)

module Output =
    type PuzzleInputOutput = { LineCount: int }
    fsi.AddPrintTransformer(fun (input:PuzzleInput) -> { LineCount = input.Lines.Length })
    let rec ``Example Part 1`` = let answer = AdventOfCode.part1 Input.example in printfn $"{nameof ``Example Part 1``}: {answer}"; answer
    let rec ``Puzzle Part 1`` = let answer = AdventOfCode.part1 Input.puzzle in printfn $"{nameof ``Puzzle Part 1``}: {answer}"; answer
    let rec ``Example Part 2`` = let answer = AdventOfCode.part2 Input.example2 in printfn $"{nameof ``Example Part 2``}: {answer}"; answer
    let rec ``Puzzle Part 2`` = let answer = AdventOfCode.part2 Input.puzzle in printfn $"{nameof ``Puzzle Part 2``}: {answer}"; answer
