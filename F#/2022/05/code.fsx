#r "nuget:FParsec"

type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    open System.Collections.Generic
    open FParsec

    type Instruction = {
        Count: int
        To: int
        From: int
    }

    let stacksParser =
        let crate =
            choice [
                skipArray 3 (skipChar ' ') |>> fun _ -> ' '
                skipChar '[' >>. letter .>> skipChar ']'
            ]

        let crateLine = attempt (sepBy crate (skipChar ' ') .>> skipNewline)

        let crates = many crateLine

        crates
        |>> fun listOfLists ->
            let crates = array2D listOfLists
            let stacks = Array.init (Array2D.length2 crates) (fun _ -> Stack<char>())

            for y in (Array2D.length2 crates - 1) .. -1 .. 0 do
                for x in (Array2D.length1 crates - 1) .. -1 .. 0 do
                    let char = Array2D.get crates x y
                    if char <> ' ' then
                        stacks[y].Push(Array2D.get crates x y)

            stacks
    let instructionParser =
        skipString "move " >>. pint32 .>> skipString " from " .>>. pint32 .>> skipString " to " .>>. pint32
        |>> (fun ((count, fromStack), toStack) -> {
            Count = count
            To = toStack - 1
            From = fromStack - 1
        })

    let parser =
        stacksParser
        .>> (skipManyTill skipAnyChar skipNewline)
        .>> skipNewline
        .>>. (sepEndBy instructionParser skipNewline)

    let parse text =
        match run parser text with
        | Success ((stacks, instructions), _, _) -> (stacks, instructions)
        | Failure (error, _, _) -> failwith error


    let part1 (puzzle:PuzzleInput) =
        let stacks, instructions = parse puzzle.FullText
        for instruction in instructions do
            for _ in 1..instruction.Count do
                stacks[instruction.From].Pop() |> stacks[instruction.To].Push

        stacks
        |> Array.map (fun stack -> stack.Peek() |> string)
        |> String.concat ""

    let part2 (puzzle:PuzzleInput) =
        let stacks, instructions = parse puzzle.FullText

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
    let rec ``Example Part 1`` = let answer = AdventOfCode.part1 Input.example in printfn $"{nameof ``Example Part 1``}: {answer}"; answer
    let rec ``Puzzle Part 1`` = let answer = AdventOfCode.part1 Input.puzzle in printfn $"{nameof ``Puzzle Part 1``}: {answer}"; answer
    let rec ``Example Part 2`` = let answer = AdventOfCode.part2 Input.example in printfn $"{nameof ``Example Part 2``}: {answer}"; answer
    let rec ``Puzzle Part 2`` = let answer = AdventOfCode.part2 Input.puzzle in printfn $"{nameof ``Puzzle Part 2``}: {answer}"; answer
