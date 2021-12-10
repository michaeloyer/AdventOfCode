module Data =
    open System.IO
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadLines
        |> Seq.map (fun line -> line.ToCharArray() |> Seq.map (string >> int))
        |> array2D
    let example = readData "example"
    let puzzle = readData "puzzle"

module Array2D =
    let tryItem x y array2d =
        try Array2D.get array2d x y  |> Some
        with | ex -> None

module Code =
    let getLowPoints array2d =
        let tryItem x y = Array2D.tryItem x y array2d |> Option.defaultValue 10
        [
            for x in Array2D.base1 array2d .. Array2D.length1 array2d do
            for y in Array2D.base2 array2d .. Array2D.length2 array2d do
                let item = tryItem x y
                if item < tryItem (x - 1) y &&
                   item < tryItem (x + 1) y &&
                   item < tryItem x (y + 1) &&
                   item < tryItem x (y - 1) then
                    yield (x, y)
        ]
    let part1 data =
        getLowPoints data
        |> List.sumBy (fun (x, y) -> (Array2D.get data x y) + 1)

    let part2 data =
        let getBasinSize (x, y) =
            let rec getBasinSize set = function
                | [] -> set |> Set.count
                | coordinate :: coordinates when Set.contains coordinate set ->
                    getBasinSize set coordinates
                | (x, y) :: coordinates ->
                    match Array2D.tryItem x y data with
                    | None
                    | Some 9 ->
                        getBasinSize set coordinates
                    | Some _ ->
                        let coordinates = [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)] @ coordinates
                        let set = Set.add (x, y) set
                        getBasinSize set coordinates

            getBasinSize Set.empty [(x, y)]

        getLowPoints data
        |> List.map getBasinSize
        |> List.sortDescending
        |> List.take 3
        |> List.reduce (*)

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

