#if INTERACTIVE
#else
module AdventOfCode2020.Day15
#endif

let example = [ 0; 3; 6 ]
let puzzle = [0;3;1;6;7;5]

let findNthTurn turnToFind numbers =
    let mapNumberAge =
        numbers
        |> Seq.indexed
        |> Seq.map (fun (index, number) -> (number, index + 1))
        |> Map.ofSeq
        
    let lastNumber = List.last numbers 
    let turn = List.length numbers + 1
    let rec nextNumber mapNumberAge turn lastNumber =
        let number = mapNumberAge
                  |> Map.tryFind lastNumber
                  |> Option.map (fun age -> turn - age - 1)
                  |> Option.defaultValue 0
        if turn = turnToFind then
            number
        else
            nextNumber (mapNumberAge |> Map.add lastNumber (turn - 1)) (turn + 1) number
        
    nextNumber mapNumberAge turn lastNumber



module Part1 =
    let solve = findNthTurn 2020

module Part2 =
    let solve = findNthTurn 30_000_000
    

Part1.solve puzzle  // 852
Part2.solve puzzle  // 6007666