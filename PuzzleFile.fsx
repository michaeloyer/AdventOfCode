[<RequireQualifiedAccess>]
module PuzzleFile

open System.IO

let private readFile f year day name =
    let filePath = Path.Combine(__SOURCE_DIRECTORY__, $"%i{year}", $"%02i{day}", $"%s{name}.txt")
    if File.Exists filePath then
        f filePath
    else
        failwith $"No Puzzle File found at {filePath}"

let lines directory day name = readFile File.ReadAllLines directory day name
let text directory day name = readFile File.ReadAllText directory day name
