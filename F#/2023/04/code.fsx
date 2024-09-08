type PuzzleInput = { FullText: string; Lines: string list }
type Numbers = {
    CardNumber: int
    Numbers: Set<int>
    WinningNumbers: Set<int>
}

type CardMatches = {
    CardNumber: int
    MatchCount: int
}
module AdventOfCode =
    open System.Text.RegularExpressions

    let readNumbers (line:string) =
        let match' =
            let pattern = "Card\s+(?<cardNumber>\d+):\s+(?<numbers>[\d\s]+)\s\|\s(?<winningNumbers>[\d\s]+)"
            Regex.Match(line, pattern)
        let getNumbers (group:string) =
            match'.Groups[group].Value.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map int
            |> Set.ofSeq
        { CardNumber = int(match'.Groups["cardNumber"].Value)
          Numbers = getNumbers "numbers"
          WinningNumbers = getNumbers "winningNumbers" }

    let countMatches numbers =
        Set.intersect numbers.Numbers numbers.WinningNumbers
        |> Set.count

    let part1 (puzzle:PuzzleInput) =
        let getNumbersPoints numbers =
            countMatches numbers
            |> fun count -> 2.0 ** (count - 1 |> float)
            |> int64

        puzzle.Lines
        |> List.sumBy (readNumbers >> getNumbersPoints)

    let part2 (puzzle:PuzzleInput) =
        let numbers =
            puzzle.Lines
            |> List.map readNumbers
            |> List.rev

        let rec loop map = function
        | [] -> map |> Map.values |> Seq.sum
        | head :: tail ->
            let count = countMatches head
            let total =
                Seq.init count (fun i ->
                    let id = head.CardNumber + i + 1
                    map |> Map.tryFind id |> Option.defaultValue 0
                )
                |> Seq.sum
            let total = total + 1
            loop (map |> Map.add head.CardNumber total) tail

        loop Map.empty numbers

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
