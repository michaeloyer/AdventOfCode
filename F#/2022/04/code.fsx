module Data =
    open System.IO
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadLines

    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
    module Set =
        let overlaps set1 set2 =
            Set.forall (fun item -> Set.contains item set2) set1

    let pairSet (pairText:string) =
        let range = pairText.Split('-')
        Set [ (int range[0])..(int range[1])]
    let part1 (data: string seq) =
        data
        |> Seq.map (fun line ->
            let pairs = line.Split(',')
            pairSet pairs[0], pairSet pairs[1]
        )
        |> Seq.filter (fun (set1, set2) ->
            Set.overlaps set1 set2 || Set.overlaps set2 set1
        )
        |> Seq.length


    let part2 (data: string seq) =
        data
        |> Seq.map (fun line ->
            let pairs = line.Split(',')
            pairSet pairs[0], pairSet pairs[1]
        )
        |> Seq.filter (fun (set1, set2) -> Set.intersect set1 set2 |> Seq.isEmpty|> not)
        |> Seq.length

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

