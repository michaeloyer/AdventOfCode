#load "../PuzzleFile.fsx"
#load "../Utils.fsx"

let example = PuzzleFile.text 2020 6 "example"
              |> String.split "\n\n"

let puzzle = PuzzleFile.text 2020 6 "puzzle"
            |> String.split "\n\n"

let sumGroup answers =
    answers
    |> String.split "\n"
    |> Seq.collect id
    |> Seq.distinct
    |> Seq.length

let part1 groupText =
    groupText
    |> Seq.sumBy sumGroup

let sumSimilarAnswerGroup answers =
    answers
    |> String.split "\n"
    |> Seq.map (fun a -> a.ToCharArray() |> Set.ofArray)
    |> Set.intersectMany
    |> Set.count

let part2 groupText =
    groupText
    |> Seq.sumBy sumSimilarAnswerGroup

let ``Part 1 example answer`` = part1 example
let ``Part 1 puzzle answer`` = part1 puzzle
let ``Part 2 example answer`` = part2 example
let ``Part 2 puzzle answer`` = part2 puzzle
