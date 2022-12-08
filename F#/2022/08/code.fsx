type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    let part1 (puzzle:PuzzleInput) =
        let array = array2D puzzle.Lines

        let top = 0
        let left = 0
        let bottom = Array2D.length1 array - 1
        let right = Array2D.length2 array - 1

        let isVisible getNumber coordinate targetCoordinate step number =
            let mutable visible = true
            let mutable coordinate = coordinate + step
            while visible && (if step = -1 then coordinate >= targetCoordinate else coordinate <= targetCoordinate) do
                visible <- getNumber coordinate < number
                coordinate <- coordinate + step
            visible

        let mutable answer = 0

        array
        |> Array2D.iteri (fun y x number ->
            answer <- answer +
                if x = left || x = right || y = top || y = bottom then
                    1
                elif
                    isVisible (fun x -> Array2D.get array y x) x left -1 number ||
                    isVisible (fun y -> Array2D.get array y x) y top -1 number ||
                    isVisible (fun x -> Array2D.get array y x) x right 1 number ||
                    isVisible (fun y -> Array2D.get array y x) y bottom 1 number then
                    1
                else
                    0
        )

        answer

    let part2 (puzzle:PuzzleInput) =
        let array = array2D puzzle.Lines

        let top = 0
        let left = 0
        let bottom = Array2D.length1 array - 1
        let right = Array2D.length2 array - 1

        let scenicScore getNumber coordinate targetCoordinate step number =
            let mutable visible = true
            let mutable coordinate = coordinate + step
            let mutable score = 0
            while visible && (if step = -1 then coordinate >= targetCoordinate else coordinate <= targetCoordinate) do
                score <- score + 1
                visible <- getNumber coordinate < number
                coordinate <- coordinate + step

            score

        let mutable answer = 0

        array
        |> Array2D.iteri (fun y x number ->
            let score =
                scenicScore (fun x -> Array2D.get array y x) x left -1 number *
                scenicScore (fun y -> Array2D.get array y x) y top -1 number *
                scenicScore (fun x -> Array2D.get array y x) x right 1 number *
                scenicScore (fun y -> Array2D.get array y x) y bottom 1 number

            if answer < score then
                answer <- score
        )

        answer

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
