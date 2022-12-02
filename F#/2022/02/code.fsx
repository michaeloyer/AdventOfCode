type Selection = Rock | Paper | Scissors

type Outcome = Win | Lose | Draw

type RoundSelection = {
    Mine: Selection
    Theirs: Selection
}

module Data =
    open System.IO
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadLines
    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
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

    let part1 data =
        data
        |> Seq.map (fun (line : string) ->
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

    let part2 data =
        data
        |> Seq.map (fun (line : string) ->
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

