#load "../../PuzzleFile.fsx"
#load "../../Utils.fsx"

let example = PuzzleFile.lines 2020 1 "example" |> Array.map int
let puzzle = PuzzleFile.lines 2020 1 "puzzle" |> Array.map int

let part1 sequence =
    let numberSet = sequence |> Set.ofSeq
    sequence
    |> Seq.choose (fun n ->
        let neededNumber = 2020 - n
        if numberSet |> Set.contains neededNumber then Some(n, neededNumber) else None)
    |> Seq.map (fun (x, y) -> x * y)
    |> Seq.distinct
    |> List.ofSeq

let ``part1 example answer`` = part1 example
let ``part1 puzzle answer`` = part1 puzzle

let part2 sequence =
    sequence
    |> Seq.permute3
    |> Seq.choose (fun (x,y,z) -> if (x + y + z) = 2020 then Some(x * y * z) else None)
    |> Seq.toList

let ``part2 example answer`` = part2 example
let ``part2 puzzle answer`` = part2 puzzle
