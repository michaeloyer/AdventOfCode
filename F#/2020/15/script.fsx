#load "../PuzzleFile.fsx"
#load "../Utils.fsx"

let getNumbers name = PuzzleFile.text 2020 15 name |> String.split "," |> Array.map int
let example = getNumbers "example"
let puzzle = getNumbers "puzzle"

// let findNthTurn turnToFind numbers =
//     let mapNumberAge =
//         numbers
//         |> Seq.indexed
//         |> Seq.map (fun (index, number) -> (number, index + 1))
//         |> Map.ofSeq

//     let lastNumber = Array.last numbers
//     let turn = Array.length numbers + 1
//     let rec nextNumber mapNumberAge turn lastNumber =
//         let number = mapNumberAge
//                   |> Map.tryFind lastNumber
//                   |> Option.map (fun age -> turn - age - 1)
//                   |> Option.defaultValue 0
//         if turn = turnToFind then
//             number
//         else
//             nextNumber (mapNumberAge |> Map.add lastNumber (turn - 1)) (turn + 1) number

//     nextNumber mapNumberAge turn lastNumber

let findNthTurn turnToFind numbers =
    let rec findNthTurn map turn number = seq {
        let previousTurn =
            map
            |> Map.tryFind number
            |> Option.defaultValue 0

        let newMap = map |> Map.add number turn
        let newTurn = turn + 1
        let newNumber = turn - previousTurn

        yield number
        yield! findNthTurn newMap newTurn newNumber
    }

    findNthTurn (numbers |> Seq.mapi (fun index item -> (index + 1, item)) |> Map.ofSeq) (numbers |> Seq.length |> (+) 1) (Seq.last numbers)
    |> Seq.skip turnToFind
    |> Seq.head

let part1 numbers = findNthTurn 2020 numbers

let part2 numbers = findNthTurn 30_000_000 numbers

let ``Part 1 example answer`` = part1 example
let ``Part 1 puzzle answer`` = part1 puzzle
let ``Part 2 example answer`` = part2 example
let ``Part 2 puzzle answer`` = part2 puzzle
