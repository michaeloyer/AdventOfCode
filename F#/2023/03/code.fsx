
type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    open System
    open System.Text.RegularExpressions

    module Array2D =
        let tryGet x y arr =
            let xLength = Array2D.length1 arr
            let yLength = Array2D.length2 arr

            if x < 0 || x >= xLength || y < 0 || y >= yLength then
                None
            else
                Some arr[x,y]

    let part1 ({ Lines = lines }:PuzzleInput) =
        let arr = Array2D.init lines.Length lines[0].Length (fun x y -> lines[y][x])

        lines
        |> Seq.indexed
        |> Seq.collect (fun (y, line) -> [
            for m in Regex.Matches(line, @"\b\d") do
                let x = m.Index
                x,y
        ])
        |> Seq.sumBy (fun (x,y) ->
            let number = Regex.Match(lines[y].Substring(x), "\\d+").Value

            let searchIndecies = seq {
                let xMin = x - 1
                let xMax = x + number.Length
                for x in xMin .. xMax do
                    yield x,(y-1)
                    yield x,(y+1)

                yield xMin, y
                yield xMax, y
            }

            let isPartNumber =
                searchIndecies |> Seq.exists (fun (x,y) ->
                    if x < 0 || y < 0 || x >= arr.GetLength(0) || y >= arr.GetLength(1) then
                        false
                    else
                        let char = arr[x,y]
                        not (Char.IsDigit char || char = '.')
                )

            if isPartNumber then
                int number
            else
                0
        )

    let part2 ({ Lines = lines }:PuzzleInput) =
        let arr = Array2D.init lines.Length lines[0].Length (fun x y -> lines[y][x])

        let startNumberIndecies =
            lines
            |> Seq.indexed
            |> Seq.collect (fun (y, line) -> [
                for m in Regex.Matches(line, @"\b\d") do
                    let x = m.Index
                    x,y
            ])

        let gridNumbers = Array2D.zeroCreate (arr.GetLength(0)) (arr.GetLength(1))

        for (index, (x, y)) in startNumberIndecies |> Seq.indexed do
            let numberText = Regex.Match(lines[y].Substring(x), "\\d+").Value
            let number = int numberText
            for i in x .. x + numberText.Length - 1 do
                gridNumbers[i, y] <- Some(index, number)

        List.sum [
            for x in 0 .. arr.GetLength(0) - 1 do
                for y in 0 .. arr.GetLength(1) - 1 do
                    let char = arr[x, y]
                    if Char.IsDigit char || char = '.' then
                        0
                    else
                        let numbers =
                            [
                                for a in x-1 .. x+1 do
                                    for b in y-1 .. y+1 do
                                        yield gridNumbers |> Array2D.tryGet a b |> Option.flatten
                            ]
                            |> List.distinct
                            |> List.choose id
                            |> List.map snd

                        if numbers.Length = 2 then
                            numbers |> List.reduce (*)
                        else
                            0
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
