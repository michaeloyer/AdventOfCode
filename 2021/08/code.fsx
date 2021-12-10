#r "nuget:FParsec"
module Data =
    open System.IO
    open FParsec

    module Parsers =
        let signalWire = many1Chars letter |>> Set
        let signalWires = sepBy1 signalWire (skipChar ' ' >>? followedBy letter)
        let entry = signalWires .>> skipString " | " .>>. signalWires
        let entries = sepEndBy entry skipNewline
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadAllText
        |> run Parsers.entries
        |> function
            | Success (entries, _, _) -> entries
            | Failure (message, _, _) -> failwith message
    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
    let part1 data =
        data
        |> List.collect (snd)
        |> List.choose (fun signal ->
            match Set.count signal with
            | 2 // 1
            | 4 // 4
            | 3 // 7
            | 7 // 8
                -> Some()
            | _ -> None
        )
        |> List.length

    module Set =
        let completes set1 set2 = Set.count (Set.intersect set1 set2) = 7
        let notCompletes set1 set2 = completes set1 set2 |> not

    let part2 data =
        let solveEntry (input, output) =
            let inputsWithLength length =
                let map = input |> List.groupBy Set.count |> Map.ofList
                length
                |> fun n -> map |> Map.tryFind n |> Option.defaultValue List.empty

            let (|One|_|) map = map |> Map.tryFind 1
            let (|Two|_|) map = map |> Map.tryFind 2
            let (|Three|_|) map = map |> Map.tryFind 3
            let (|Four|_|) map = map |> Map.tryFind 4
            let (|Five|_|) map = map |> Map.tryFind 5
            let (|Six|_|) map = map |> Map.tryFind 6
            let (|Seven|_|) map = map |> Map.tryFind 7
            let (|Eight|_|) map = map |> Map.tryFind 8
            let (|Nine|_|) map = map |> Map.tryFind 9
            let (|Zero|_|) map = map |> Map.tryFind 0

            let solveOne _ = (inputsWithLength 2) |> List.tryHead
            let solveTwo = function
                | Three three & Five five -> inputsWithLength 5 |> List.except [three; five] |> List.tryHead
                | Four n -> inputsWithLength 5 |> List.tryFind (Set.completes n)
                | Five n -> inputsWithLength 5 |> List.tryFind (Set.completes n)
                | Nine n -> inputsWithLength 5 |> List.tryFind (not << Set.isSuperset n)
                | _ -> None
            let solveThree = function
                | One n -> inputsWithLength 5 |> List.tryFind (Set.isSubset n)
                | Seven n -> inputsWithLength 5 |> List.tryFind (Set.isSubset n)
                | _ -> None
            let solveFour _ = inputsWithLength 4 |> List.tryHead
            let solveFive = function
                | Two n -> inputsWithLength 5 |> List.tryFind (Set.completes n)
                | Six n -> inputsWithLength 5 |> List.tryFind (Set.isSuperset n)
                | _ -> None
            let solveSix = function
                | Seven n -> inputsWithLength 6 |> List.tryFind (not << Set.isSubset n)
                | _ -> None
            let solveSeven _ = inputsWithLength 3 |> List.tryHead
            let solveEight _ = inputsWithLength 7 |> List.tryHead
            let solveNine = function
                | Four n -> inputsWithLength 6 |> List.tryFind (Set.isSubset n)
                | _ -> None
            let solveZero = function
                | Nine nine & Six six -> inputsWithLength 6 |> List.except [nine; six] |> List.tryHead
                | Five n -> inputsWithLength 6 |> List.tryFind (Set.completes n)
                | _ -> None

            let solveN n map =
                if map |> Map.containsKey n then
                    None
                else
                    let f =
                        match n with
                        | 0 -> solveZero
                        | 1 -> solveOne
                        | 2 -> solveTwo
                        | 3 -> solveThree
                        | 4 -> solveFour
                        | 5 -> solveFive
                        | 6 -> solveSix
                        | 7 -> solveSeven
                        | 8 -> solveEight
                        | 9 -> solveNine
                        | n -> invalidOp $"Invalid number {n}"
                    f map
                    |> Option.map (fun value -> Map.add n value map)

            let allNumbers = [0..9]
            let rec solveMap = function
                | (map, []) -> map
                | map, n :: numbers ->
                    match solveN n map with
                    | Some map -> solveMap (map, allNumbers)
                    | None -> solveMap (map, numbers)

            let map =
                solveMap (Map.empty, allNumbers)
                |> Map.toSeq
                |> Seq.map (fun (key, value) -> value, key)
                |> Map.ofSeq

            // printfn $"Competed Map: %A{map}"

            output
            |> List.map (fun o ->
                match Map.tryFind o map with
                | Some i -> string i
                | None ->
                    let outputMap =
                        map
                        |> Map.toList
                        |> List.map snd
                        |> List.groupBy (function | 8 | 4 | 7 | 1 -> 1 | 2 | 3 | 5 -> 2 | 0 | 6 | 9 -> 3 | _ -> 0)
                        |> List.sortBy fst
                        |> List.map snd
                    failwith $"Unsolved key: %A{o} in map %A{outputMap}; %A{map}")
            |> String.concat ""
            |> int

        data |> List.indexed |> List.sumBy (fun (i, entry) -> let sum = solveEntry entry in printfn "%i;%i" i sum; sum)

module Answers =
    open Data
    open Code

    let ``Example Part 1`` = part1 example
    let ``Puzzle Part 1`` = part1 puzzle
    let ``Example Part 2`` = part2 example
    let ``Puzzle Part 2`` = part2 puzzle

    do
        printfn $"{nameof(``Example Part 1``)}: {``Example Part 1``}"
        printfn $"{nameof(``Puzzle Part 1``)}: {``Puzzle Part 1``}"
        printfn $"{nameof(``Example Part 2``)}: {``Example Part 2``}"
        printfn $"{nameof(``Puzzle Part 2``)}: {``Puzzle Part 2``}"

