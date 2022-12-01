module Data =
    open System.IO
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadLines
        |> Seq.map (fun line ->
            match line.Split('-') with
            | [| left; right |] -> (left, right)
            | _ -> invalidOp $"invalid line: {line}"
        )
    let example1 = readData "example1"
    let example2 = readData "example2"
    let example3 = readData "example3"
    let puzzle = readData "puzzle"

module Code =
    type Path = {
        Steps: string list
        Set: Set<string>
        ReplaySmallCaveCount: int
    }

    let getPaths data =
        let pathMap =
            [ yield! data |> List.ofSeq
              yield! data |> Seq.map (fun (fst,snd) -> (snd,fst)) ]
            |> List.groupBy fst
            |> List.map (fun (key, keyGroups) -> (key, keyGroups |> List.map snd))
            |> Map.ofList

        fun start ->
            pathMap
            |> Map.tryFind start
            |> Option.defaultValue List.empty

    let getStartPaths replaySmallCaveCount getPaths =
        getPaths "end"
        |> List.map (fun step ->
            let path = [step; "end" ]
            { Steps = path; Set = Set path; ReplaySmallCaveCount = replaySmallCaveCount })

    let part1 data =
        let getPaths = getPaths data

        let rec processPaths completedPaths = function
            | [] -> completedPaths
            | path :: remainingPaths when path.Steps.Head = "start" ->
                processPaths (List.append [path.Steps] completedPaths) remainingPaths
            | path :: paths ->
                let potentialPaths =
                    getPaths path.Steps.Head
                    |> List.choose (fun step ->
                        if not (System.Char.IsLower step[0] && path.Set.Contains step) then
                            Some { Steps = step :: path.Steps;
                                   Set = Set.add step path.Set
                                   ReplaySmallCaveCount = 0 }
                        else
                            None
                    )

                processPaths completedPaths (List.append potentialPaths paths)

        let startPaths = getStartPaths 0 getPaths

        processPaths [] startPaths
        |> List.length

    let part2 data =
        let getPaths = getPaths data

        let rec processPaths completedPaths = function
            | [] -> completedPaths
            | path :: remainingPaths when path.Steps.Head = "start" ->
                processPaths (List.append [path.Steps] completedPaths) remainingPaths
            | path :: remainingPaths when path.Steps.Head = "end" ->
                processPaths completedPaths remainingPaths
            | path :: paths ->
                let potentialPaths =
                    getPaths path.Steps.Head
                    |> List.choose (fun step ->
                        if System.Char.IsLower step[0] &&
                           path.Set.Contains step &&
                           path.ReplaySmallCaveCount = 0 then
                            None
                        elif System.Char.IsLower step[0] && path.Set.Contains step then
                            Some { Steps = step :: path.Steps;
                                   Set = Set.add step path.Set;
                                   ReplaySmallCaveCount = path.ReplaySmallCaveCount - 1}
                        else
                            Some { Steps = step :: path.Steps;
                                   Set = Set.add step path.Set;
                                   ReplaySmallCaveCount = path.ReplaySmallCaveCount }
                    )

                processPaths completedPaths (List.append potentialPaths paths)

        let startPaths = getStartPaths 1 getPaths

        processPaths [] startPaths
        |> List.length

module Answers =
    open Data
    open Code

    let ``Example 1 Part 1`` = part1 example1
    let ``Example 2 Part 1`` = part1 example2
    let ``Example 3 Part 1`` = part1 example3
    let ``Puzzle Part 1`` = part1 puzzle
    let ``Example 1 Part 2`` = part2 example1
    let ``Example 2 Part 2`` = part2 example2
    let ``Example 3 Part 2`` = part2 example3
    let ``Puzzle Part 2`` = part2 puzzle

    do
        printfn $"{nameof(``Example 1 Part 1``)}: {``Example 1 Part 1``}"
        printfn $"{nameof(``Example 2 Part 1``)}: {``Example 2 Part 1``}"
        printfn $"{nameof(``Example 3 Part 1``)}: {``Example 3 Part 1``}"
        printfn $"{nameof(``Puzzle Part 1``)}: {``Puzzle Part 1``}"
        printfn $"{nameof(``Example 1 Part 2``)}: {``Example 1 Part 2``}"
        printfn $"{nameof(``Example 2 Part 2``)}: {``Example 2 Part 2``}"
        printfn $"{nameof(``Example 3 Part 2``)}: {``Example 3 Part 2``}"
        printfn $"{nameof(``Puzzle Part 2``)}: {``Puzzle Part 2``}"

