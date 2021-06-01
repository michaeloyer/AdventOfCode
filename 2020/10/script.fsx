#load "../../PuzzleFile.fsx"
#load "../../Utils.fsx"

let loadNumbers = PuzzleFile.lines 2020 10 >> Seq.map int >> List.ofSeq
let example1 = loadNumbers "example1"
let example2 = loadNumbers "example2"
let puzzle = loadNumbers "puzzle"

module Part1 =
    type State = { Ones: int; Threes: int; LastNumber: int }
    let solve list =
        list
        |> Seq.sort
        |> Seq.fold (
            fun state number ->
                match number - state.LastNumber with
                | 1 -> { state with Ones = state.Ones + 1; LastNumber = number }
                | 3 -> { state with Threes = state.Threes + 1; LastNumber = number }
                | _ -> failwith "Invalid Solution"
        ) { Ones = 0; Threes = 1; LastNumber = 0 }
        |> fun state -> state.Ones * state.Threes

module Part2 =
    type State = { Groups: int64 list list; CurrentList: int64 list; LastNumber: int64 }

    let solve arr =
        [0L] @ List.map int64 arr
        |> List.sort
        |> List.pairwise
        |> List.map (fun (a, b) -> b - a)
        |> List.fold
               (fun (list, count) number -> if number = 3L then (list @ [count], 1) else (list, count + 1))
               ([], 1)
        |> fun (list, count) -> list @ [count]
        |> Seq.map (function
            | 5 -> 7L
            | 4 -> 4L
            | 3 -> 2L
            | _ -> 1L)
        |> Seq.reduce (*)

let ``Part 1 example answer`` = Part1.solve example1
let ``Part 1 puzzle answer`` = Part1.solve puzzle
let ``Part 2 example answer`` = Part2.solve example2
let ``Part 2 puzzle answer`` = Part2.solve puzzle
