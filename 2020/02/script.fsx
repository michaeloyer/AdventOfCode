#load "../../PuzzleFile.fsx"
#load "../../Utils.fsx"

#r "nuget: FSharp.Text.RegexProvider"

open System
open FSharp.Text.RegexProvider

let example = PuzzleFile.lines 2020 2 "example"
let puzzle = PuzzleFile.lines 2020 2 "puzzle"

type Input = Regex< "(?<Min>\d+)\-(?<Max>\d+) (?<Character>[A-Za-z]): (?<Password>\w+)" >

let part1 sequence =
    sequence |>
    Seq.filter(fun i ->
        let input = Input().TypedMatch(i)
        let min = int input.Min.Value
        let max = int input.Max.Value
        let password = input.Password.Value
        let character = input.Character.Value
        let count = Regex.Matches(password, character).Count

        min <= count && count <= max
        )
    |> Seq.length

let ``Part 1 example answer`` = part1 example
let ``Part 1 puzzle answer`` = part1 puzzle

let part2 sequence =
    sequence |>
    Seq.filter(fun i ->
        let input = Input().TypedMatch(i)
        let first = int input.Min.Value
        let second = int input.Max.Value
        let password = input.Password.Value
        let character = char input.Character.Value

        (password.[first - 1] = character) <> (password.[second - 1] = character)
        )
    |> Seq.length

let ``Part 2 example answer`` = part2 example
let ``Part 2 puzzle answer`` = part2 puzzle
