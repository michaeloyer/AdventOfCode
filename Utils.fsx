[<AutoOpen>]
module Utils

let between min max value = min <= value && value <= max

[<RequireQualifiedAccess>]
module Seq =
    let permute2 sequence =
        let isequence = Seq.indexed sequence
        seq {
            for (i, x) in isequence do
            for (j, y) in isequence do
                if (i < j) then
                    yield x, y
        }

    let permute3 sequence = seq {
            let isequence = Seq.indexed sequence

            for (i, x) in isequence do
            for (j, y) in isequence do
            for (k, z) in isequence do
                if (i < j && j < k) then
                    yield x, y, z
        }

[<RequireQualifiedAccess>]
module String =
    let split (separator:string) (text:string) =
        text.Split(separator, System.StringSplitOptions.TrimEntries ||| System.StringSplitOptions.RemoveEmptyEntries)

[<RequireQualifiedAccess>]
module Array2D =
    let tryItem x y arr2d =
        let lower1 = Array2D.base1 arr2d
        let lower2 = Array2D.base2 arr2d
        let upper1 = Array2D.length1 arr2d - lower1 - 1
        let upper2 = Array2D.length2 arr2d - lower2 - 1

        if lower1 <= x && x <= upper1 &&
           lower2 <= y && y <= upper2 then
            Some arr2d.[x, y]
        else
            None

    let flattenX arr2d =
        [|
            for x in { (Array2D.base1 arr2d)..(Array2D.length1 arr2d - 1) } do
            for y in { (Array2D.base2 arr2d)..(Array2D.length2 arr2d - 1) } do
                yield arr2d.[x, y]
        |]

    let flattenY arr2d =
        [|
            for y in { (Array2D.base2 arr2d)..(Array2D.length2 arr2d - 1) } do
            for x in { (Array2D.base1 arr2d)..(Array2D.length1 arr2d - 1) } do
                yield arr2d.[x, y]
        |]
