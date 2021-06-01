#load "../../PuzzleFile.fsx"
#load "../../Utils.fsx"

#r "nuget: FParsec"

open FParsec

let example = PuzzleFile.text 2020 14 "example"
let example2 = PuzzleFile.text 2020 14 "example2"
let puzzle = PuzzleFile.text 2020 14 "puzzle"

let toBinary (number:int64) =
    System.Convert.ToString(number, 2).PadLeft(36, '0')

let toNumber binary =
    System.Convert.ToInt64(binary, 2)

type Instruction =
    | Mask of string
    | Mem of int32 * int32

let defaultBinary = System.String('0', 36)

let maskParser: Parser<Instruction,unit> = pstring "mask = " >>. regex "(?:0|1|X){36}" |>> Mask
let memParser: Parser<Instruction,unit> =
    pstring "mem[" >>. pint32 .>> pstring "] = " .>>. pint32 |>> Mem

let instructionParser = maskParser <|> memParser
let instructionsParser = sepEndBy instructionParser spaces

let part1 text =
    let applyMask mask binary =
        Seq.zip mask binary
        |> Seq.map (fun (m, b) -> (if m = 'X' then b else m) |> string)
        |> String.concat ""

    run instructionsParser text |>
        function
        | Failure (message, _, _) -> failwith message
        | Success (instructions, _, _) ->
            instructions
            |> Seq.fold (fun (addresses, mask) instruction ->
                match instruction with
                | Mask newMask -> (addresses, newMask)
                | Mem (address, value) ->
                    (addresses |> Map.add address (value |> int64 |> toBinary |> applyMask mask), mask)
            ) (Map.empty, "")
            |> fst
            |> Map.toSeq
            |> Seq.sumBy (snd >> toNumber)

let part2 text =
    let applyMask mask binary =
        Seq.zip mask binary
        |> Seq.map (fun (m, b) ->
            match m with
            | '0' -> string b
            | m -> string m
        )
        |> String.concat ""

    let getAddresses (mask:string) =
        let rec getAddresses charList lists =
            match charList with
            | [] -> lists
            | 'X' :: chars ->
                getAddresses chars [
                    yield! lists |> List.map (fun list -> List.append list ['0'])
                    yield! lists |> List.map (fun list -> List.append list ['1'])
                ]
            | char :: chars ->
                getAddresses chars (lists |> List.map (fun list -> List.append list [char]))

        getAddresses (mask.ToCharArray() |> List.ofArray) [[]]
        |> List.map (fun list -> list |> List.map string |> String.concat "")

    run instructionsParser text |>
        function
        | Failure (message, _, _) -> failwith message
        | Success (instructions, _, _) ->
            instructions
            |> Seq.fold (fun (addresses, mask) instruction ->
                match instruction with
                | Mask newMask -> (addresses, newMask)
                | Mem (address, value) ->
                    let addresses =
                        address |> int64 |> toBinary
                        |> applyMask mask
                        |> getAddresses
                        |> List.map toNumber
                        |> List.fold (fun addresses address -> addresses |> Map.add address value) addresses
                    addresses, mask
            ) (Map.empty, "")
            |> fst
            |> Map.toSeq
            |> Seq.sumBy (snd >> int64)

let ``Part 1 example answer`` = part1 example
let ``Part 1 puzzle answer`` = part1 puzzle
let ``Part 2 example answer`` = part2 example2
let ``Part 2 puzzle answer`` = part2 puzzle
