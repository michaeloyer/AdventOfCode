[<AutoOpen>]
module AdventOfCode2020.Utils

let between min max value = min <= value && value <= max

module String =
    let splitWithOptions (options:System.StringSplitOptions) (splitOn:string seq) (text:string) =
        text.Split(splitOn |> Seq.toArray, options)

    let split splitOn text =
        (splitOn, text) ||> splitWithOptions (System.StringSplitOptions.RemoveEmptyEntries |||
                                              System.StringSplitOptions.TrimEntries)
        
    let splitLines text =
        text |> split ["\n"; "\r"]
                       
module Seq =
    let permute2 sequence =
        let isequence = Seq.indexed sequence
        seq {
            for (i, x) in isequence do
            for (j, y) in isequence do
                if (i < j) then
                    yield x,y
        }
    let permute3 sequence =
        let isequence = Seq.indexed sequence
        seq {
            for (i, x) in isequence do
            for (j, y) in isequence do
            for (k, z) in isequence do
                if (i < j && j < k) then
                    yield x,y,z
        }
