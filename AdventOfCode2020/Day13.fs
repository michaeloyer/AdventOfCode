#if INTERACTIVE
#load "Utils.fs"
open AdventOfCode2020.Utils
#else
module AdventOfCode2020.Day13
#endif

let example = """939
7,13,x,x,59,x,31,19"""

let puzzle = """1014511
17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,643,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29,x,433,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,19"""

module Part1 =
    let solve text =
        let arr = String.splitLines text
        let timestamp = int (arr.[0])
        let buses = arr.[1]
                    |> String.split [","]
                    |> Seq.choose (fun str -> if str = "x" then None else Some(int str))
        
        buses
        |> Seq.map (fun bus ->
            let time = bus - (timestamp % bus) + timestamp
            (bus, time - timestamp)
        )
        |> Seq.minBy snd
        |> fun (bus, time) -> bus * time

module Part2 =
    let solve text =
        let rec findNextBus timestamp offset bus step =
            if (timestamp + offset) % bus = 0L then
                timestamp
            else
                findNextBus (timestamp + step) offset bus step
                
        String.splitLines text
        |> Array.item 1
        |> String.split [","]
        |> Seq.indexed
        |> Seq.choose (fun (i, str) -> if str = "x" then None else Some(int64 i, int64 str))
        |> Seq.fold (fun (timestamp, step) (index, bus) ->
            let timestamp = findNextBus timestamp index bus step
            printfn "Timestamp: %s; BusId: %s; step: %s" (timestamp |> string) (bus |> string) (step |> string)
            (timestamp, step * bus)
        ) (0L, 1L) 