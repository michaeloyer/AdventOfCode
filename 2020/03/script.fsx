#load "../../PuzzleFile.fsx"

let example = PuzzleFile.lines 2020 3 "example"
let puzzle = PuzzleFile.lines 2020 3 "puzzle"

let traverse down right lines =
    lines
    |> Seq.skip down
    |> Seq.indexed
    |> Seq.choose (fun (index, item) -> if index % down = 0 then Some(item) else None)
    |> Seq.fold (fun (position, count) line ->
        let position = (position + right) % String.length line
        let count = count + if line.[position] = '#' then 1 else 0
        (position, count)
    ) (0, 0)
    |> snd

let part1 sequence =
    traverse 1 3 sequence

let ``Part 1 example answer`` = part1 example
let ``Part 1 puzzle answer`` = part1 puzzle

let part2 sequence =
    seq {
        traverse 1 1 sequence
        traverse 1 3 sequence
        traverse 1 5 sequence
        traverse 1 7 sequence
        traverse 2 1 sequence
    }
    |> Seq.reduce (*)

let ``Part 2 example answer`` = part2 example
let ``Part 2 puzzle answer`` = part2 puzzle
