#load "../../PuzzleFile.fsx"
#load "../../Utils.fsx"

type PuzzleData = {
    timestamp: int
    buses: string array
}

let getPuzzleData name =
    PuzzleFile.text 2020 13 name
    |> String.split "\n"
    |> fun array -> {
        timestamp = int array.[0]
        buses = array.[1] |> String.split ","
    }

let example = getPuzzleData "example"

let puzzle = getPuzzleData "puzzle"

let part1 data =
    let timestamp = data.timestamp
    let buses = data.buses |> Array.choose (fun str -> if str = "x" then None else Some(int str))

    buses
    |> Seq.map (fun bus ->
        let time = bus - (data.timestamp % bus) + data.timestamp
        (bus, time - timestamp)
    )
    |> Seq.minBy snd
    |> fun (bus, time) -> bus * time

let part2 data =
    let rec findNextBus timestamp offset bus step =
        if (timestamp + offset) % bus = 0L then
            timestamp
        else
            findNextBus (timestamp + step) offset bus step

    data.buses
    |> Seq.indexed
    |> Seq.choose (fun (i, str) -> if str = "x" then None else Some(int64 i, int64 str))
    |> Seq.fold (fun (timestamp, step) (index, bus) ->
        let timestamp = findNextBus timestamp index bus step
        (timestamp, step * bus)
    ) (0L, 1L)
    |> fst

let ``Part 1 example answer`` = part1 example
let ``Part 1 puzzle answer`` = part1 puzzle
let ``Part 2 example answer`` = part2 example
let ``Part 2 puzzle answer`` = part2 puzzle
