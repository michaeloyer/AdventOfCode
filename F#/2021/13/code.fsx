#r "nuget:FParsec"


type Fold =
    | Vertical of int
    | Horizontal of int

type Coordinate = { X: int; Y: int }

module Data =
    open System.IO
    open FParsec

    module private Parsers =
        let coordinate =
            pint32 .>> skipChar ',' .>>. pint32
            |>> fun (y, x) -> { X = x; Y = y }

        let fold =
            let fold axis foldType = skipString $"fold along {axis}=" >>. pint32 |>> foldType
            choice [
                fold "y" Vertical
                fold "x" Horizontal
            ]

        let puzzle =
            sepEndBy coordinate newline .>>
            newline .>>.
            sepEndBy fold newline .>>
            eof
            |>> (fun (coordinates, folds) ->
                let length1 =
                    coordinates
                    |> List.map (fun c -> c.X)
                    |> List.max
                    |> (+) 1

                let length2 =
                    coordinates
                    |> List.map (fun c -> c.Y)
                    |> List.max
                    |> (+) 1

                let array = Array2D.zeroCreate<bool> length1 length2

                for { X = x; Y = y } in coordinates do
                    array[x,y] <- true

                (array, folds)
            )

    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadAllText
        |> run Parsers.puzzle
        |> function
            | Success (t, _, _) -> t
            | Failure (message, _, _) -> failwith message
    let example() = readData "example"
    let puzzle() = readData "puzzle"

module Code =
    let safePick x y array =
        if x >= Array2D.length1 array ||
           y >= Array2D.length2 array ||
           x < Array2D.base1 array ||
           y < Array2D.base2 array then
            false
        else
            Array2D.get array x y

    let foldOn foldType (array: bool [,]) =
        match foldType with
        | Vertical x ->
            let array1 = array[0..(x-1),0..^0]
            let array2 = array[(x+1)..^0,0..^0]
            let newArray =
                Array2D.zeroCreate
                    (max (Array2D.length1 array1) (Array2D.length1 array2))
                    (Array2D.length2 array1)

            let lengthX = Array2D.length1 array1
            newArray |> Array2D.mapi (fun x y _ -> safePick x y array1 || safePick (lengthX - x - 1) y array2)
        | Horizontal y ->
            let array1 = array[0..^0,0..(y-1)]
            let array2 = array[0..^0,(y+1)..^0]
            let newArray =
                Array2D.zeroCreate
                    (Array2D.length1 array1)
                    (max (Array2D.length2 array1) (Array2D.length2 array2))

            let lengthY = Array2D.length2 array1
            newArray |> Array2D.mapi (fun x y _ -> safePick x y array1 || safePick (x) (lengthY - y - 1) array2)

    let part1 (grid, folds) =
        let fold = List.head folds

        let mutable count = 0

        grid
        |> foldOn fold
        |> Array2D.iter (fun b -> if b then count <- count + 1)

        count

    let part2 (grid, folds) =
        let newGrid = (grid, folds) ||> List.fold (fun grid fold -> foldOn fold grid)

        let builder = System.Text.StringBuilder()
        for x in Array2D.base1 newGrid .. Array2D.length1 newGrid - 1 do
            for y in Array2D.base2 newGrid .. Array2D.length2 newGrid - 1 do
                builder.Append(if newGrid[x,y] then "#" else " ") |> ignore
            builder.AppendLine() |> ignore

        string builder

module Answers =
    open Data
    open Code

    let ``Example Part 1`` = part1 (example())
    let ``Puzzle Part 1`` = part1 (puzzle())
    let ``Example Part 2`` = part2 (example())
    let ``Puzzle Part 2`` = part2 (puzzle())

    do
        printfn $"{nameof(``Example Part 1``)}:\n{``Example Part 1``}"
        printfn $"{nameof(``Puzzle Part 1``)}:\n{``Puzzle Part 1``}"
        printfn $"{nameof(``Example Part 2``)}:\n{``Example Part 2``}"
        printfn $"{nameof(``Puzzle Part 2``)}:\n{``Puzzle Part 2``}"

