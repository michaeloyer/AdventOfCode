module Data =
    open System.IO
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadAllText
        |> fun text -> text.Split(',')
        |> Array.map int
    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
    let getLeastByFuel getFuel data =
        seq { 0 .. (Array.max data) }
        |> Seq.map getFuel
        |> Seq.min

    let part1 data =
        let getFuel n = data |> Array.sumBy (fun i -> abs (i - n))
        data |> getLeastByFuel getFuel

    let part2 data =
        let getFuel =
            let mappedFuelCost =
                (0, seq { 1.. (Array.max data) }) ||> Seq.scan (+)
                |> Seq.indexed
                |> Map.ofSeq

            let findFuelCost amount =
                Map.find amount mappedFuelCost

            fun n -> data |> Array.sumBy (fun i -> abs (i - n) |> findFuelCost)

        data |> getLeastByFuel getFuel

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

