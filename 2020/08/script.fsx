#load "../../PuzzleFile.fsx"

open FParsec

#r "nuget: FParsec"

open FParsec

let example = PuzzleFile.text 2020 8 "example"

let puzzle = PuzzleFile.text 2020 8 "puzzle"

type Instruction =
    | Accumulator of int
    | Jump of int
    | Noop of int

let noOperationParser: Parser<Instruction,unit> = skipString "nop" >>. spaces >>. pint32 |>> Noop
let accumulatorParser: Parser<Instruction,unit> = skipString "acc" >>. spaces >>. pint32 |>> Accumulator
let jumpParser: Parser<Instruction,unit> = skipString "jmp" >>.spaces >>. pint32 |>> Jump

let instructionParser = noOperationParser <|> accumulatorParser <|> jumpParser

let allInstructionsParser = sepBy instructionParser skipNewline .>> eof

type State = { Position: int
               Total: int
               History: int Set }

let getInstructions text =
    run allInstructionsParser text
    |> function
        | Success (result, _, _) -> result
        | Failure _ -> failwith "Parser Failure"

let part1 text =
    let instructions = getInstructions text

    let rec findLoopInstruction { Position = position; Total = total; History = history } =
        if history |> Set.contains position then
            total
        else

        let history = history |> Set.add position
        match instructions |> List.item position with
        | Noop _ -> findLoopInstruction { Position = position + 1; Total = total; History = history }
        | Accumulator i -> findLoopInstruction { Position = position + 1; Total = total + i; History = history }
        | Jump i -> findLoopInstruction { Position = position + i; Total = total; History = history }

    let answer = findLoopInstruction { Position = 0; Total = 0; History = Set.empty }
    answer

let part2 text =
    let instructions = getInstructions text
    let flippedInstructions =
        instructions
        |> Seq.indexed
        |> Seq.choose (function
            | (i, Noop n) -> Some (i, Jump n)
            | (i, Jump n) -> Some (i, Noop n)
            | _ -> None)

    let rec doInstructions instructions { Position = position; Total = total; History = history } =
        if history |> Set.contains position then
            None
        elif position = (List.length instructions) - 1 then
            Some total
        else

        let doInstructions = doInstructions instructions
        let history = history |> Set.add position
        match instructions.[position] with
        | Noop _ ->
            doInstructions { Position = position + 1; Total = total; History = history }
        | Accumulator i ->
            doInstructions { Position = position + 1; Total = total + i; History = history }
        | Jump i ->
            doInstructions { Position = position + i; Total = total; History = history }

    let emptyState =
        { Position = 0
          Total = 0
          History = Set.empty }

    let getNewInstructions i instruction =
        instructions.[..(i - 1)] @ [instruction] @ instructions.[(i + 1)..]

    let answer =
        flippedInstructions
        |> Seq.choose (fun (index, flippedInstruction) ->
            let instructions = getNewInstructions index flippedInstruction
            doInstructions instructions emptyState
        )
        |> Seq.head

    answer

let ``Part 1 example answer`` = part1 example
let ``Part 1 puzzle answer`` = part1 puzzle
let ``Part 2 example answer`` = part2 example
let ``Part 2 puzzle answer`` = part2 puzzle
