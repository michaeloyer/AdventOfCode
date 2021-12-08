#r "nuget:FParsec"

module Data =
    open System.IO
    open FParsec

    type Square =
        | Checked
        | Unchecked of int

    type GameBoard = Square[,]
    type GameData = {
        NumbersToDraw: int list
        GameBoards: GameBoard list
    }

    type ResultingBoards =
        | Boards of GameBoard list
        | Bingo of GameBoard

    module Parsers =
        let drawnNumbers = sepEndBy1 pint32 (skipChar ',')
        let board =
            let listOf5 a b c d e = [a;b;c;d;e]
            let row =
                let n = spaces >>. pint32 |>> Unchecked
                pipe5 n n n n n listOf5

            let row = row .>> skipNewline
            pipe5 row row row row row listOf5
            |>> array2D

        let boards = sepEndBy board skipNewline
        let gameData =
            drawnNumbers .>> skipNewline .>>. boards .>> eof
            |>> (fun (numbers, boards) ->
                { NumbersToDraw = numbers
                  GameBoards = boards })

    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadAllText
        |> run Parsers.gameData
        |> function
            | Success(gameData, _, _) -> gameData
            | Failure(error, _, _) -> failwith error

    let example() = readData "example"
    let puzzle() = readData "puzzle"

module Array2D =
    let tryFind item array =
        let startX = Array2D.base1 array
        let startY = Array2D.base2 array
        let endX = Array2D.length1 array - startX - 1
        let endY = Array2D.length2 array - startY - 1

        let rec tryFind = function
        | (_, y) when y > endY -> None
        | (x, y) when x > endX -> tryFind (0, y + 1)
        | (x, y) when Array2D.get array x y <> item -> tryFind (x + 1, y)
        | (x, y) -> Some (x, y)

        tryFind (startX, startY)

    let toArray array = [|
        let startX = Array2D.base1 array
        let startY = Array2D.base2 array
        let endX = Array2D.length1 array - startX - 1
        let endY = Array2D.length2 array - startY - 1

        for x in { startX .. endX } do
            for y in { startY .. endY } do
                yield Array2D.get array x y
    |]


module Code =
    open Data

    let checkBoard x y board =
        let allChecked = List.forall (function | Checked -> true | _ -> false)
        let startX = Array2D.base1 board
        let endX = Array2D.length1 board - 1
        let startY = Array2D.base2 board
        let endY = Array2D.length2 board - 1

        allChecked [ for x in startX..endX -> board[x,y]] ||
        allChecked [ for y in startY..endY -> board[x,y]]

    let markBoard number board =
        Array2D.tryFind (Unchecked number) board
        |> Option.map (fun (x, y) -> board[x,y] <- Checked; (x,y))

    let boardToValue board number =
        board
        |> Array2D.toArray
        |> Array.sumBy (function | Checked -> 0 | Unchecked n -> n)
        |> (*) number

    let part1 { NumbersToDraw=numbersToDraw; GameBoards=gameBoards } =
        let markBoards number gameBoards =
            let mutable foundBoard = None
            for board in gameBoards do
                if Option.isNone foundBoard then
                    match markBoard number board with
                    | Some (x, y) ->
                        if checkBoard x y board then
                            foundBoard <- Some board
                    | None -> ()

            match foundBoard with
            | Some board -> Bingo board
            | None -> Boards gameBoards

        let rec drawNumbers = function
            | [] -> failwith "Never got bingo"
            | number :: restOfNumbers ->
                match markBoards number gameBoards with
                | Bingo board -> board, number
                | Boards _ -> drawNumbers restOfNumbers

        let winningBoard, winningNumber = drawNumbers numbersToDraw

        boardToValue winningBoard winningNumber

    let part2 { NumbersToDraw=numbersToDraw; GameBoards=gameBoards } =
        let maybeStillPlaying number board =
            match markBoard number board with
            | Some (x, y) -> if checkBoard x y board then None else Some board
            | None -> Some board

        let rec findLastBoard = function
        | Bingo board, lastNumber :: _ -> boardToValue board lastNumber
        | Boards [board], number :: numbers ->
            match markBoard number board with
            | Some (x, y)->
                if checkBoard x y board then
                    findLastBoard (Bingo board, [number])
                else
                    findLastBoard (Boards [board], numbers)
            | None -> findLastBoard (Boards [board], numbers)
        | Boards boards, number :: numbers ->
            let remainingBoards = boards |> List.choose (maybeStillPlaying number)

            findLastBoard (Boards remainingBoards, numbers)
        | _ -> failwith "Invalid State"

        findLastBoard (Boards gameBoards, numbersToDraw)

module Answers =
    open Data
    open Code

    let ``Example Part 1`` = part1 (example())
    let ``Puzzle Part 1`` = part1 (puzzle())
    let ``Example Part 2`` = part2 (example())
    let ``Puzzle Part 2`` = part2 (puzzle())

    do
        printfn $"{nameof(``Example Part 1``)}: {``Example Part 1``}"
        printfn $"{nameof(``Puzzle Part 1``)}: {``Puzzle Part 1``}"
        printfn $"{nameof(``Example Part 2``)}: {``Example Part 2``}"
        printfn $"{nameof(``Puzzle Part 2``)}: {``Puzzle Part 2``}"
