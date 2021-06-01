#load "../../PuzzleFile.fsx"
#load "../../Utils.fsx"

#r "nuget: FParsec"

open FParsec

let example = PuzzleFile.text 2020 12 "example"
let puzzle = PuzzleFile.text 2020 12 "puzzle"

type CardinalDirection =
    | North
    | South
    | West
    | East

type TurnDirection =
    | Left
    | Right

type MoveInstruction =
    | CardinalDirection of CardinalDirection * int
    | Forward of int

module Part1 =
    type Ship = {
        Facing: CardinalDirection
        X: int
        Y: int
    }

    let newShip() = { Facing = East; X = 0; Y = 0 }

    let moveShip direction amount (ship: Ship) =
        match direction with
        | North -> { ship with Y = ship.Y + amount }
        | South -> { ship with Y = ship.Y - amount }
        | East -> { ship with X = ship.X + amount }
        | West -> { ship with X = ship.X - amount }

    let moveShipForward amount ship =
        moveShip ship.Facing amount ship

    let rec turnShip direction amount ship =
        if amount <= 0 then ship
        else

        let nowFacing =
            match (direction, ship.Facing) with
            | Left, North -> West
            | Left, West -> South
            | Left, South -> East
            | Left, East -> North
            | Right, North -> East
            | Right, East -> South
            | Right, South -> West
            | Right, West -> North

        turnShip direction (amount - 1) { ship with Facing = nowFacing }

    let instructionParser: Parser<(Ship -> Ship), unit> =
        (pchar 'F' >>. pint32 |>> moveShipForward)
        <|> (pchar 'N' >>. pint32 |>> moveShip North)
        <|> (pchar 'S' >>. pint32 |>> moveShip South)
        <|> (pchar 'E' >>. pint32 |>> moveShip East)
        <|> (pchar 'W' >>. pint32 |>> moveShip West)
        <|> (pchar 'L' >>. pint32 |>> fun amount -> turnShip Left (amount / 90 % 4))
        <|> (pchar 'R' >>. pint32 |>> fun amount -> turnShip Right (amount / 90 % 4))


    let solve text =
        run (sepEndBy instructionParser spaces) text
        |> function
            | Success (instructions, _, _) -> Seq.fold (fun ship instruction -> instruction ship) (newShip()) instructions
            | Failure (message, _, _) -> failwith message
        |> fun ship -> abs ship.X + abs ship.Y

module Part2 =
    type Ship = {
        X: int
        Y: int
    }

    type WayPoint = {
        X: int
        Y: int
    }

    let newShip() : Ship = { X = 0; Y = 0 }
    let newWayPoint() : WayPoint = { X = 10; Y = 1 }

    let moveWayPoint direction amount (waypoint:WayPoint) (ship:Ship) =
        let newWayPoint =
            match direction with
            | North -> { waypoint with Y = waypoint.Y + amount }
            | South -> { waypoint with Y = waypoint.Y - amount }
            | West -> { waypoint with X = waypoint.X - amount }
            | East -> { waypoint with X = waypoint.X + amount }

        newWayPoint, ship

    let rec rotateWaypoint direction amount (waypoint: WayPoint) (ship:Ship) =
        if amount <= 0 then
            waypoint, ship
        else

        let newWaypoint =
            match direction with
            | Left -> { X = waypoint.Y * -1; Y = waypoint.X }
            | Right -> { X = waypoint.Y; Y = waypoint.X * -1 }

        rotateWaypoint direction (amount - 1) newWaypoint ship


    let moveShip amount (waypoint:WayPoint) (ship:Ship) =
        let ship = { ship with X = ship.X + (waypoint.X * amount)
                               Y = ship.Y + (waypoint.Y * amount) }

        waypoint, ship


    let instructionParser: Parser<WayPoint -> Ship -> WayPoint * Ship, unit> =
        (pchar 'F' >>. pint32 |>> moveShip)
        <|> (pchar 'N' >>. pint32 |>> moveWayPoint North)
        <|> (pchar 'S' >>. pint32 |>> moveWayPoint South)
        <|> (pchar 'E' >>. pint32 |>> moveWayPoint East)
        <|> (pchar 'W' >>. pint32 |>> moveWayPoint West)
        <|> (pchar 'L' >>. pint32 |>> fun amount -> rotateWaypoint Left (amount / 90 % 4))
        <|> (pchar 'R' >>. pint32 |>> fun amount -> rotateWaypoint Right (amount / 90 % 4))

    let solve text =
        run (sepEndBy instructionParser spaces) text
        |> function
            | Success (instructions, _, _) ->
                Seq.fold (fun (waypoint, ship) instruction -> instruction waypoint ship) (newWayPoint(), newShip()) instructions
            | Failure (message, _, _) -> failwith message
        |> fun (_, ship) -> abs ship.X + abs ship.Y

let ``Part 1 example answer`` = Part1.solve example
let ``Part 1 puzzle answer`` = Part1.solve puzzle
let ``Part 2 example answer`` = Part2.solve example
let ``Part 2 puzzle answer`` = Part2.solve puzzle
