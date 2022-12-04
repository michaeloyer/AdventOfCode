module Data =
    open System.IO
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadLines

    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
    let parseRanges (text:string) =
        let numbers = text.Split(',', '-')
        (Set [ (int numbers[0])..(int numbers[1])],
         Set [ (int numbers[2])..(int numbers[3])])

    let part1 data =
        data
        |> Seq.map parseRanges
        |> Seq.filter (fun (set1, set2) ->
            Set.isSubset set1 set2 || Set.isSuperset set1 set2
        )
        |> Seq.length

    let part2 data =
        data
        |> Seq.map parseRanges
        |> Seq.filter (fun (set1, set2) ->
            Set.exists (fun item -> Set.contains item set2) set1
        )
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

