#load "../../PuzzleFile.fsx"
#load "../../Utils.fsx"

#r "nuget: FParsec"

open FParsec

let example1 = PuzzleFile.text 2020 7 "example1"
let example2 = PuzzleFile.text 2020 7 "example1"
let puzzle = PuzzleFile.text 2020 7 "puzzle"

let wordParser: Parser<string,unit> = (manyChars letter) .>> spaces
let colorParser = manyTill wordParser (skipString "bag" .>> optional (skipChar 's'))
                  |>> String.concat " "
let contentsColorParser = pint32 .>> spaces .>>. colorParser

let colorSentenceParser: Parser<(string * (int32 * string) list),unit> =
    let filledBagParser = sepBy contentsColorParser (skipString ", ")
    let emptyBagParser = stringReturn "no other bags" List.empty

    colorParser
    .>> skipString " contain "
    .>>. (emptyBagParser <|> filledBagParser)
    .>> pchar '.'

let colorSentencesParser = sepEndBy colorSentenceParser skipNewline

let part1 text =
    let colorContentsDictionaryParser =
        colorSentencesParser
        |>> (
            Seq.collect (fun (container, contents) ->
                contents |> Seq.map (fun (_, content) -> (content, container))
            )
            >> Seq.groupBy fst
            >> Seq.map (fun (contentKey, contentKeyAndContainers) ->
                let containers = contentKeyAndContainers |> Seq.map snd
                (contentKey, containers)
            )
            >> dict
        )

    let colorContentsDictionary =
        run colorContentsDictionaryParser text
        |> function
            | Failure _ -> failwith "Parsing Failure"
            | Success (dictionary, _, _) -> dictionary

    let rec getPotentialContainers containerSet contentList =
        contentList
        |> Seq.collect (fun content ->
            match colorContentsDictionary.TryGetValue(content) with
            | (true, contents) -> contents
            | (false, _) -> Seq.empty)
        |> Seq.except containerSet
        |> Seq.toList
        |> function
            | [] -> containerSet
            | containers ->
                let containerSet = containerSet |> Set.union (Set.ofList containers)
                getPotentialContainers containerSet containers

    getPotentialContainers Set.empty ["shiny gold"]
    |> Set.count

let part2 text =
    let containerDictionaryParser =
        colorSentencesParser
        |>> dict

    let containerDictionary =
        run containerDictionaryParser text
        |> function
            | Success (dictionary, _, _) -> dictionary
            | Failure _ -> failwith "Invalid Parser"

    let rec countBags container =
        match containerDictionary.TryGetValue container with
            | (false, _) -> 0
            | (true, []) -> 0
            | (true, countsAndColors) ->
                countsAndColors
                |> Seq.sumBy (fun (count, color) ->
                    count + (count * countBags color))

    let answer = countBags "shiny gold"
    answer

let ``Part 1 example answer`` = part1 example1
let ``Part 1 puzzle answer`` = part1 puzzle

let ``Part 2 example answer`` = part2 example2
let ``Part 2 puzzle answer`` = part2 puzzle
