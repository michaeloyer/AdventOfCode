#load "../../PuzzleFile.fsx"
#load "../../Utils.fsx"

let loadNumbers name = PuzzleFile.lines 2020 9 name |> Seq.map int64 |> List.ofSeq
let example = loadNumbers "example"
let puzzle = loadNumbers "puzzle"


let part1 preamble numbers =
    let hasSum numbers total =
        Seq.permute2 numbers
        |> Seq.exists (fun (x, y) -> x + y = total)

    numbers
    |> List.indexed
    |> List.skip preamble
    |> List.choose (fun (index, number) ->
        let list = numbers.[(index - preamble)..(index - 1)]
        if not(hasSum list number) then
            Some number
        else
            None
    )
    |> List.head

let part2 preamble numbers =
    let weakness = part1 preamble numbers
    let rec findContiguousSum sum total foundNumbers numbers =
        match numbers, total with
        | _, total when total = sum -> Some foundNumbers
        | _, total when total > sum -> None
        | [], _ -> None
        | n :: ns, _ -> findContiguousSum sum (total + n) (foundNumbers @ [n]) ns

    Seq.scanBack (fun number lst -> lst @ [number]) numbers []
    |> Seq.map List.rev
    |> Seq.filter (not << List.isEmpty)
    |> Seq.choose (findContiguousSum weakness 0L [])
    |> Seq.head
    |> fun numbers -> (List.min numbers) + (List.max numbers)

let ``Part 1 example answer`` = part1 5 example
let ``Part 1 puzzle answer`` = part1 25 puzzle
let ``Part 2 example answer`` = part2 5 example
let ``Part 2 puzzle answer`` = part2 25 puzzle
