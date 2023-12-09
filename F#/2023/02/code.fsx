type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    type Cubes = {
        Red: int
        Green: int
        Blue: int
    }

    type Game = {
        Id: int
        Rounds: Cubes list
    }

    let readGame (line:string) =
        let id, roundsText =
            let arr = line.Split(": ")
            int (arr[0].Substring("Game ".Length)), arr[1]

        let readRound (round:string) =
            ({ Red = 0; Green = 0; Blue = 0 }, round.Split(", "))
            ||> Array.fold (
                fun state cube ->
                    let count, color =
                        let arr = cube.Split(" ")
                        int arr[0], arr[1]

                    match color with
                    | "red" -> { state with Red = count }
                    | "green" -> { state with Green = count }
                    | "blue" -> { state with Blue = count }
                    | _ -> failwith $"Invalid color {color}"
            )

        let rounds = roundsText.Split("; ") |> Array.map readRound |> Array.toList

        { Id = id; Rounds = rounds }

    let part1 (puzzle:PuzzleInput) = List.sum [
        for line in puzzle.Lines do
            let game = readGame line
            let allRoundsPossible =
                game.Rounds
                |> List.forall (fun round ->
                    round.Red <= 12 &&
                    round.Green <= 13 &&
                    round.Blue <= 14)

            if allRoundsPossible then
                yield game.Id
    ]

    let part2 (puzzle:PuzzleInput) = List.sum [
        for line in puzzle.Lines do
            let game = readGame line

            let minimumCubes =
                game.Rounds
                |> List.reduce (fun round1 round2 ->
                    { Red = max round1.Red round2.Red
                      Green = max round1.Green round2.Green
                      Blue = max round1.Blue round2.Blue }
                )
            yield minimumCubes.Red * minimumCubes.Green * minimumCubes.Blue
    ]

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
