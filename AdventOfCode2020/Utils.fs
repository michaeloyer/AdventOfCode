[<AutoOpen>]
module AdventOfCode2020.Utils

type StringOrChar = | Char of char | String of string


module String =
    let splitWithOptions (options:System.StringSplitOptions) (splitOn:string seq) (text:string) =
        text.Split(splitOn |> Seq.toArray, options)

    let split splitOn text =
        (splitOn, text) ||> splitWithOptions (System.StringSplitOptions.RemoveEmptyEntries |||
                                              System.StringSplitOptions.TrimEntries)
        
    let splitLines text =
        text |> split ["\n"; "\r"]
                       
