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
        
module Array2D =
    let tryGet arr2d x y =
        let length1, length2 = Array2D.length1 arr2d, Array2D.length2 arr2d
        
        if x |> between 0 (length1 - 1) && y |> between 0 (length2 - 1) then
            Some (Array2D.get arr2d x y)
        else
            None
    
    let flattenX arr2d =
        [|
            for x in { 0..(Array2D.length1 arr2d - 1) } do
            for y in { 0..(Array2D.length2 arr2d - 1) } ->
                arr2d.[x, y]
        |]
        
    let flattenY arr2d =
        [|
            for y in { 0..(Array2D.length2 arr2d - 1) } do
            for x in { 0..(Array2D.length1 arr2d - 1) } ->
                arr2d.[x, y]
        |]