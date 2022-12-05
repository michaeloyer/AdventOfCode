type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    open System
    open System.Collections.Generic
    open System.Text.RegularExpressions

    type Instruction = {
        Count: int
        To: int
        From: int
    }

    let parseStacks lines =
        lines
        |> List.takeWhile (fun line -> not (String.exists Char.IsDigit line))
        |> List.map (fun line -> [
            for i in 1..4..(line.Length) do
                yield line[i]
        ])
        |> fun listOfLists ->
            let crates = array2D listOfLists
            let stacks = Array.init (Array2D.length2 crates) (fun _ -> Stack<char>())

            for y in (Array2D.length2 crates - 1) .. -1 .. 0 do
                for x in (Array2D.length1 crates - 1) .. -1 .. 0 do
                    let char = Array2D.get crates x y
                    if char <> ' ' then
                        stacks[y].Push(Array2D.get crates x y)

            stacks

    let parseInstructions lines =
        let parseInstruction line =
            let regexMatch = Regex.Match(line, "move (?<count>\\d+) from (?<from>\\d+) to (?<to>\\d+)")
            { Count = int regexMatch.Groups["count"].Value
              To = int regexMatch.Groups["to"].Value - 1
              From = int regexMatch.Groups["from"].Value - 1 }

        lines
        |> List.skipWhile (not << String.IsNullOrEmpty)
        |> List.skip 1
        |> List.map parseInstruction

    let parse lines =
        parseStacks lines, parseInstructions lines

    let part1 (puzzle:PuzzleInput) =
        let stacks, instructions = parse puzzle.Lines
        for instruction in instructions do
            for _ in 1..instruction.Count do
                stacks[instruction.From].Pop() |> stacks[instruction.To].Push

        stacks
        |> Array.map (fun stack -> stack.Peek() |> string)
        |> String.concat ""

    let part2 (puzzle:PuzzleInput) =
        let stacks, instructions = parse puzzle.Lines

        for instruction in instructions do
            let crates = Stack [
                for _ in 1..instruction.Count do
                    yield stacks[instruction.From].Pop()
            ]

            for crate in crates do
                stacks[instruction.To].Push(crate)

        stacks
        |> Array.map (fun stack -> stack.Peek() |> string)
        |> String.concat ""

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
