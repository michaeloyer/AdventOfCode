type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    let part1 (puzzle:PuzzleInput) =
        let array = array2D puzzle.Lines

        let mutable answer = 0
        for x in 0 .. (Array2D.length1 array - 1) do
            for y in 0 .. (Array2D.length2 array - 1) do
                if x = 0 || y = 0 || x = Array2D.length1 array - 1 || y = Array2D.length2 array - 1 then
                    answer <- answer + 1
                else
                    let number = array[y,x]
                    let visibleLeft =
                        let mutable visible = true
                        for x in (x - 1).. -1 .. 0 do
                            visible <- visible && array[y,x] < number
                        visible
                    let visibleRight =
                        let mutable visible = true
                        for x in (x + 1)..(Array2D.length1 array - 1) do
                            visible <- visible && array[y,x] < number
                        visible
                    let visibleTop =
                        let mutable visible = true
                        for y in (y - 1).. -1 .. 0 do
                            visible <- visible && array[y,x] < number
                        visible
                    let visibleBottom =
                        let mutable visible = true
                        for y in (y + 1)..(Array2D.length2 array - 1) do
                            visible <- visible && array[y,x] < number
                        visible

                    if visibleTop || visibleBottom || visibleLeft || visibleRight then
                        answer <- answer + 1
        answer

    let part2 (puzzle:PuzzleInput) =
        let array = array2D puzzle.Lines

        List.max [
            for x in 0 .. (Array2D.length1 array - 1) do
                for y in 0 .. (Array2D.length2 array - 1) do
                    let number = array[y,x]
                    let leftScore =
                        let mutable score = 0
                        let mutable visible = true
                        for x in (x - 1).. -1 .. 0 do
                            if visible then
                                score <- score + 1
                            visible <- visible && array[y,x] < number
                        score
                    let rightScore =
                        let mutable score = 0
                        let mutable visible = true
                        for x in (x + 1)..(Array2D.length1 array - 1) do
                            if visible then
                                score <- score + 1
                            visible <- visible && array[y,x] < number
                        score
                    let topScore =
                        let mutable score = 0
                        let mutable visible = true
                        for y in (y - 1).. -1 .. 0 do
                            if visible then
                                score <- score + 1
                            visible <- visible && array[y,x] < number
                        score
                    let bottomScore =
                        let mutable score = 0
                        let mutable visible = true
                        for y in (y + 1)..(Array2D.length2 array - 1) do
                            if visible then
                                score <- score + 1
                            visible <- visible && array[y,x] < number
                        score

                    yield (leftScore * rightScore * topScore * bottomScore)
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
