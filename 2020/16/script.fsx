#load "../../PuzzleFile.fsx"
#load "../../Utils.fsx"
#load "../../ParseUtils.fsx"

#r "nuget: FParsec"

open FParsec
open System.Collections.Generic


type Field = {
    title: string
    numbers: Set<int>
}

type PuzzleData = {
    fields: Field array
    yourTicket: int array
    nearbyTickets: int array list
}


let numberRange : Parser<_, unit> = pint32 .>> skipChar '-' .>>. pint32 |>> (fun (min, max) -> Set({min..max}))
let numberRanges = numberRange .>> skipString " or " .>>. numberRange |>> fun (set1, set2) -> Set.union set1 set2
let field =
    (manyCharsTill (letter <|> pchar ' ') (skipString ": ")) .>>. numberRanges
    |>> fun (title, numbers) -> { title = title; numbers = numbers }

let fields = sepEndBy field skipNewline |>> Array.ofList

let ticket = sepBy pint32 (skipChar ',') |>> Array.ofList

let yourTicket = skipString "your ticket:" >>. skipNewline >>. ticket
let nearbyTickets = skipString "nearby tickets:" >>. skipNewline >>. sepEndBy ticket skipNewline

let puzzleData =
    tuple3 (fields .>> skipNewline) (yourTicket .>> skipNewline .>> skipNewline) nearbyTickets
    |>> fun (fields, yourTicket, nearbyTickets) -> { fields = fields; yourTicket = yourTicket; nearbyTickets = nearbyTickets }

let getPuzzleData text =
    run puzzleData text
    |> function
        | Success (puzzleData, _, _) -> puzzleData
        | Failure (message, _, _) -> failwith message

let example = PuzzleFile.text 2020 16 "example" |> getPuzzleData
let puzzle = PuzzleFile.text 2020 16 "puzzle" |> getPuzzleData
let example2 = PuzzleFile.text 2020 16 "example2" |> getPuzzleData

let part1 puzzleData =
    puzzleData
    |> fun { fields = fields; yourTicket = yourTicket; nearbyTickets = nearbyTickets } ->
        let validNumberSet = fields |> Seq.collect (fun field -> field.numbers) |> Set.ofSeq
        nearbyTickets
        |> Seq.collect id
        |> Seq.filter (fun number -> not(validNumberSet |> Set.contains number))
        |> Seq.sum

let part2 puzzleData =
    let { fields = fields; yourTicket = yourTicket; nearbyTickets = nearbyTickets } = puzzleData

    let numberFieldNamesMap =
        fields
        |> Seq.collect (fun field -> field.numbers |> Seq.map (fun number -> (number, field.title)))
        |> Seq.groupBy (fun (number, _) -> number)
        |> Seq.map (fun (number, tuples) -> number, (tuples |> Seq.map snd |> Set.ofSeq))
        |> Map

    let ticketArray2d = array2D nearbyTickets

    let getFieldNames number =
        let set =
            Map.tryFind number numberFieldNamesMap
            |> Option.defaultValue Set.empty
        set

    let getFieldNameByRow row =
        let rec getFieldNameByRow names row =
            match Set.count names with
            | 1 -> names
            | 0 ->
                let names = row |> List.head |> getFieldNames
                getFieldNameByRow names row
            | _ ->
            match row with
            | [] -> names
            | number :: row ->
                let names = Set.intersect (getFieldNames number) names
                getFieldNameByRow names row

        getFieldNameByRow Set.empty row

    let rec figureOutSets numberOfSingleSets sets =
        if sets |> List.forall (fun set -> Set.count set = 1) then
            sets
        elif sets |> List.filter (fun set -> Set.count set = 1) |> List.length = numberOfSingleSets then
            sets
        else
            let figuredOutLabels =
                sets
                |> List.filter (fun set -> Set.count set = 1)
                |> Seq.collect id
                |> Set.ofSeq

            sets
            |> List.map (fun set -> if Set.count set = 1 then set else Set.difference set figuredOutLabels)
            |> figureOutSets (Set.count figuredOutLabels)

    let fieldNameSets = [
        for rowNumber in (Array2D.base2 ticketArray2d)..((Array2D.length2 ticketArray2d) - 1) do
            let row = ticketArray2d.[*,rowNumber] |> List.ofArray
            yield getFieldNameByRow row
    ]

    fieldNameSets
    |> figureOutSets 0
    // |> List.map Seq.head

    // fieldNames
    // |> List.indexed
    // |> List.choose (fun (index, fieldName) ->
    //     if fieldName.StartsWith("departure") then Some(yourTicket.[index]) else None)
    // |> List.sum


// let ``Part 1 example answer`` = part1 example //71
// let ``Part 1 puzzle answer`` = part1 puzzle // 26941
let ``Part 2 example answer`` = part2 example2
let ``Part 2 puzzle answer`` = part2 puzzle |> List.filter (fun set -> Set.count set = 1)

// run (fields .>> skipNewline .>> yourTicket .>> skipNewline .>>. nearbyTickets) example
