#load "../../PuzzleFile.fsx"
#load "../../Utils.fsx"

let example = PuzzleFile.lines 2020 5 "example"
let puzzle = PuzzleFile.lines 2020 5 "puzzle"

let getSeatId (boardingPass:string) =
    let getUpperHalf (min, max) =
        (max - ((max - min + 1) / 2) + 1, max)

    let getLowerHalf (min, max) =
        (min, max - ((max - min + 1) / 2))

    let rec getBoardingPassSlice left right boardingPass range =
        match boardingPass with
        | [c] when c = left -> fst range
        | [c] when c = right -> snd range
        | c :: cs when c = left -> getBoardingPassSlice left right cs (range |> getLowerHalf)
        | c :: cs when c = right -> getBoardingPassSlice left right cs (range |> getUpperHalf)
        | c :: _ -> failwithf "Invalid Boarding Pass character: %c" c
        | _ -> failwith "Empty Boarding Pass"

    let getRow (boardingPass:string) =
        getBoardingPassSlice 'F' 'B' (boardingPass.[..6].ToCharArray() |> List.ofArray) (0, 127)
    let getColumn (boardingPass:string) =
        getBoardingPassSlice 'L' 'R' (boardingPass.[7..].ToCharArray() |> List.ofArray) (0, 7)

    let row = getRow boardingPass
    let column = getColumn boardingPass

    row * 8 + column

let part1 boardingPasses =
    boardingPasses
    |> Seq.map getSeatId
    |> Seq.max

let part2 boardingPasses =
    let seatIds =
        boardingPasses
        |> Seq.map getSeatId
        |> Seq.sort
        |> List.ofSeq

    List.sum [(List.head seatIds)..(List.last seatIds)] - List.sum seatIds

let ``Part 1 example answer`` = part1 example
let ``Part 1 puzzle answer`` = part1 puzzle
let ``Part 2 puzzle answer`` = part2 puzzle
