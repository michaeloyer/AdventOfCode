#load "../PuzzleFile.fsx"
#load "../Utils.fsx"

type Seat =
    | Floor
    | Empty
    | Occupied

let getSeats lines =
    Array2D.init (Array.length lines) (String.length lines.[0])
        (fun x y -> lines.[x].[y] |> function
            | 'L' -> Empty
            | '#' -> Occupied
            | _ -> Floor )

let example = PuzzleFile.lines 2020 11 "example" |> getSeats
let puzzle = PuzzleFile.lines 2020 11 "puzzle" |> getSeats

let tryGetSeat = function
    | Some seat -> seat
    | None -> Floor

type ChangedState =
    | Starting
    | Unchanged
    | Changed

let countOccupiedSeats seats =
    let mutable count = 0
    seats |> Array2D.iter (function | Occupied -> count <- count + 1 | _ -> ())
    count

let rec shiftSeats f (state, seats) =
    match state with
    | Starting | Changed ->
        seats |> f |> shiftSeats f
    | Unchanged -> seats

let part1 seats =
    let getAdjacentSeats x y seats =
        [
            for i in { x - 1 .. x + 1 } do
            for j in { y - 1 .. y + 1 } do
                if (i <> x || j <> y) then
                    Array2D.tryItem i j seats
        ]
        |> List.choose id

    let countOccupiedAdjacentSeats x y seats =
        getAdjacentSeats x y seats
        |> Seq.sumBy (function | Occupied -> 1 | _ -> 0)

    let changeSeat seats x y = function
        | Floor -> Floor
        | Empty ->
            if countOccupiedAdjacentSeats x y seats = 0 then
                Occupied
            else
                Empty
        | Occupied ->
            if countOccupiedAdjacentSeats x y seats >= 4 then
                Empty
            else
                Occupied

    let changeSeats seats =
        let newSeats = seats |> Array2D.mapi (changeSeat seats)
        if seats = newSeats then
            (Unchanged, seats)
        else
            (Changed, newSeats)

    shiftSeats changeSeats (Starting, seats)
    |> countOccupiedSeats

let part2 seats =
    let rec getViewableSeat x y left top seats =
        match seats |> Array2D.tryItem (x + left) (y + top) with
        | Some (Occupied) -> Occupied
        | Some (Empty) -> Empty
        | None -> Floor
        | Some (Floor) -> getViewableSeat (x + left) (y + top) left top seats

    let getAdjacentSeats x y seats = seq {
        for i in -1..1 do
        for j in -1..1 do
        if not (i = 0 && j = 0) then
            yield getViewableSeat x y i j seats
    }

    let countOccupiedAdjacentSeats x y seats =
        getAdjacentSeats x y seats
        |> Seq.sumBy (function | Occupied -> 1 | _ -> 0)

    let changeSeat seats x y = function
        | Floor -> Floor
        | Empty ->
            if countOccupiedAdjacentSeats x y seats = 0 then
                Occupied
            else
                Empty
        | Occupied ->
            if countOccupiedAdjacentSeats x y seats >= 5 then
                Empty
            else
                Occupied

    let changeSeats seats =
        let newSeats = seats |> Array2D.mapi (changeSeat seats)
        if seats = newSeats then
            (Unchanged, seats)
        else
            (Changed, newSeats)

    shiftSeats changeSeats (Starting, seats)
    |> countOccupiedSeats


let ``Part 1 example answer`` = part1 example
let ``Part 1 puzzle answer`` = part1 puzzle
let ``Part 2 example answer`` = part2 example
let ``Part 2 puzzle answer`` = part2 puzzle
