type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    open System
    open System.Text.RegularExpressions

    type PartNumber = {
        Index: int
        Value: int
    }

    type GridItem =
        | PartNumber of PartNumber
        | Gear
        | Empty

    let getPartNumberMap ({ Lines = lines }:PuzzleInput) =
        lines
        |> Seq.indexed
        |> Seq.collect (fun (y, line) -> [
            for regexMatch in Regex.Matches(line, @"\d+") do
                y, regexMatch

        ])
        |> Seq.mapi (fun index (y, regexMatch) -> [
            let partNumber = {
                Index = index
                Value = int regexMatch.Value
            }

            for x in regexMatch.Index .. (regexMatch.Index + regexMatch.Length - 1) do
                yield (x, y), partNumber
        ])
        |> Seq.collect id
        |> Map

    module Grid =
        let create ({ Lines = lines } as puzzle) =
            let partNumberMap = getPartNumberMap puzzle

            Array2D.init lines.Length lines[0].Length (fun x y ->
                match lines[y][x] with
                | c when Char.IsDigit c -> PartNumber(Map.find(x,y) partNumberMap)
                | '.' -> Empty
                | _ -> Gear
            )

        let getItem x y arr =
            let xLength = Array2D.length1 arr
            let yLength = Array2D.length2 arr

            if x < 0 || x >= xLength || y < 0 || y >= yLength then
                Empty
            else
                arr[x,y]

        let getGearPartNumbers grid =
            let gearCoordinates = seq {
                for x in 0 .. Array2D.length1 grid - 1 do
                    for y in 0 .. Array2D.length2 grid - 1 do
                        match Array2D.get grid x y with
                        | Gear -> yield x,y
                        | _ -> ()
            }

            let gearPartNumbers = [
                for (x,y) in gearCoordinates do
                    [
                        for x in x-1..x+1 do
                            for y in y-1..y+1 do
                                match getItem x y grid with
                                | PartNumber partNumber -> partNumber
                                | _ -> ()
                    ]
                    |> List.distinct
            ]

            gearPartNumbers

    let part1 (puzzle:PuzzleInput) =
        Grid.create puzzle
        |> Grid.getGearPartNumbers
        |> Seq.collect id
        |> Seq.distinct
        |> Seq.sumBy (fun partNumber -> partNumber.Value)

    let part2 (puzzle:PuzzleInput) =
        Grid.create puzzle
        |> Grid.getGearPartNumbers
        |> Seq.sumBy (fun partNumbers ->
            partNumbers
            |> List.distinct
            |> function
                | [a;b]-> a.Value * b.Value
                | _ -> 0
        )

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
