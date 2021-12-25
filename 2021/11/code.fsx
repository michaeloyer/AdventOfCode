type Octopus =
    | Octopus of int
    | Flash
    | Flashed

module Data =
    open System.IO
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadLines
        |> Seq.map (fun line ->
            line.ToCharArray() |> Seq.map (string >> int >> Octopus)
        )
        |> array2D
    let example = readData "example"
    let puzzle = readData "puzzle"

module Array2D =
    let tryItem x y array =
        try Some (Array2D.get array x y)
        with _ -> None

module Code =
    let increment = function
        | Flash -> Flashed
        | Flashed -> Flashed
        | Octopus n when n >= 9 -> Flash
        | Octopus n -> Octopus (n + 1)

    let getSurroundingCoordinates x y = [
        let centerCoordinate = (x, y)
        for i in -1 .. 1 do
        for j in -1 .. 1 do
            let coordinate = (x + i, y + j)
            if coordinate <> centerCoordinate then
                yield coordinate
    ]

    let getAllCoordinates data = [
        for x in Array2D.base1 data .. Array2D.length1 data do
        for y in Array2D.base2 data .. Array2D.length2 data do
            yield x, y
    ]

    let step data =
        let (|FoundOctopus|FlashedOctopus|InvalidCoordinate|) (x, y) =
            match data |> Array2D.tryItem x y with
            | None -> InvalidCoordinate
            | Some Flash
            | Some Flashed -> FlashedOctopus
            | Some octopus -> FoundOctopus ((x, y), octopus)

        let rec step = function
        | [] ->
            let mutable flashCount = 0
            data |> Array2D.iter (function | Flash | Flashed -> flashCount <- flashCount + 1 | _ -> ())
            let data = data |> Array2D.map (function | Flash | Flashed -> Octopus 0 | octopus -> octopus)
            (flashCount, data)
        | InvalidCoordinate :: remaining
        | FlashedOctopus :: remaining ->
            step remaining
        | FoundOctopus ((x, y), octopus) :: remaining ->
            match increment octopus with
            | Flashed -> step remaining
            | Flash ->
                Array2D.set data x y Flash
                step [ yield! getSurroundingCoordinates x y
                       yield! remaining ]
            | octopus ->
                Array2D.set data x y octopus
                step remaining

        step (getAllCoordinates data)

    let print d =
        for x in Array2D.base1 d .. Array2D.length1 d - 1 do
            for y in Array2D.base2 d .. Array2D.length2 d - 1 do
                match Array2D.get d x y with
                | Flash
                | Flashed -> printf "X"
                | Octopus x -> printf "%i" x

            printfn ""
        printfn ""

    let part1 data =
        let mutable data = Array2D.copy data
        let mutable flashCount = 0

        for _ in 1 .. 100 do
            let count, newData = step data
            flashCount <- flashCount + count
            data <- newData

        flashCount

    let part2 data =
        let mutable data = Array2D.copy data

        let rec isAllZero data =
            let rec isAllZero x y data =
                if y = Array2D.length2 data then
                    true
                elif x = Array2D.length1 data then
                    isAllZero 0 (y+1) data
                elif Array2D.get data x y <> Octopus 0 then
                    false
                else
                    isAllZero (x+1) y data
            isAllZero 0 0 data

        let rec step' iterations = function
            | data when isAllZero data -> iterations
            | data ->
                let (_, newData) = step data
                step' (iterations + 1) newData


        step' 0 data

module Answers =
    open Data
    open Code

    let ``Example Part 1`` = part1 example
    let ``Puzzle Part 1`` = part1 puzzle
    let ``Example Part 2`` = part2 example
    let ``Puzzle Part 2`` = part2 puzzle

    do
        printfn $"{nameof(``Example Part 1``)}: {``Example Part 1``}"
        // printfn $"{nameof(``Puzzle Part 1``)}: {``Puzzle Part 1``}"
        printfn $"{nameof(``Example Part 2``)}: {``Example Part 2``}"
        printfn $"{nameof(``Puzzle Part 2``)}: {``Puzzle Part 2``}"

