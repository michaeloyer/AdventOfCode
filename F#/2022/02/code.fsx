type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    type Selection = Rock | Paper | Scissors

    type Outcome = Win | Lose | Draw

    type RoundSelection = {
        Mine: Selection
        Theirs: Selection
    }

    let score round =
        let selectionScore =
            match round.Mine with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3
        let victoryScore =
            match round.Mine, round.Theirs with
            | Rock, Scissors
            | Scissors, Paper
            | Paper, Rock -> 6
            | mine, theirs when mine = theirs -> 3
            | _ -> 0

        selectionScore + victoryScore

    let part1 (puzzle:PuzzleInput) =
        puzzle.Lines
        |> Seq.map (fun line ->
            let roundLine = line.Split(' ')
            { Theirs =
                match roundLine[0] with
                | "A" -> Rock
                | "B" -> Paper
                | "C" -> Scissors
                | _ -> failwith "Invalid Theirs"
              Mine =
                match roundLine[1] with
                | "X" -> Rock
                | "Y" -> Paper
                | "Z" -> Scissors
                | _ -> failwith "Invalid Mine" }
        )
        |> Seq.sumBy score

    let part2 (puzzle:PuzzleInput) =
        puzzle.Lines
        |> Seq.map (fun line ->
            let roundLine = line.Split(' ')
            let theirs =
                match roundLine[0] with
                | "A" -> Rock
                | "B" -> Paper
                | "C" -> Scissors
                | _ -> failwith "Invalid Theirs"
            let outcome =
                match roundLine[1] with
                | "X" -> Lose
                | "Y" -> Draw
                | "Z" -> Win
                | _ -> failwith "Invalid Outcome"

            let mine =
                match theirs, outcome with
                | Scissors, Draw
                | Paper, Win
                | Rock, Lose
                    -> Scissors
                | Paper, Draw
                | Rock, Win
                | Scissors, Lose
                    -> Paper
                | Rock, Draw
                | Scissors, Win
                | Paper, Lose
                    -> Rock

            { Mine = mine; Theirs = theirs }
        )
        |> Seq.sumBy score

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
