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
                       
