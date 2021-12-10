module Data =
    open System.IO
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadLines
        |> Seq.map (fun line ->
            line
        )
    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =
    open System.Collections.Generic
    type Stack<'T> with
        member this.TryPeek() =
            if this.Count > 0 then
                Some(this.Peek())
            else
                None
    let part1 data =
        let findIllegalCharacter line =
            let (|LeftBracket|_|) char =
                match char with
                | '{' | '[' | '(' | '<' -> Some char
                | _ -> None

            let rec findIllegalCharacter (stack:Stack<_>) characterList =
                match stack.TryPeek(), characterList with
                | _, [] -> None
                | _, LeftBracket char :: characterList ->
                    stack.Push(char)
                    findIllegalCharacter stack characterList
                | Some '{', '}' :: characterList ->
                    stack.Pop() |> ignore
                    findIllegalCharacter stack characterList
                | Some '(', ')' :: characterList ->
                    stack.Pop() |> ignore
                    findIllegalCharacter stack characterList
                | Some '<', '>' :: characterList ->
                    stack.Pop() |> ignore
                    findIllegalCharacter stack characterList
                | Some '[', ']' :: characterList ->
                    stack.Pop() |> ignore
                    findIllegalCharacter stack characterList
                | _, illegalCharacter :: _ -> Some illegalCharacter
            findIllegalCharacter (Stack<_>()) (line |> Seq.toList)

        data
        |> Seq.choose findIllegalCharacter
        |> Seq.sumBy (function
            | ')' -> 3
            | ']' -> 57
            | '}' -> 1197
            | '>' -> 25137
            | _ -> 0
        )

    let part2 data =
        let getIncompleteLine line =
            let (|LeftBracket|_|) char =
                match char with
                | '{' | '[' | '(' | '<' -> Some char
                | _ -> None

            let rec getIncompleteLine (stack:Stack<_>) characterList =
                match stack.TryPeek(), characterList with
                | _, [] -> Some (stack)
                | _, LeftBracket char :: characterList ->
                    stack.Push(char)
                    getIncompleteLine stack characterList
                | Some '{', '}' :: characterList ->
                    stack.Pop() |> ignore
                    getIncompleteLine stack characterList
                | Some '(', ')' :: characterList ->
                    stack.Pop() |> ignore
                    getIncompleteLine stack characterList
                | Some '<', '>' :: characterList ->
                    stack.Pop() |> ignore
                    getIncompleteLine stack characterList
                | Some '[', ']' :: characterList ->
                    stack.Pop() |> ignore
                    getIncompleteLine stack characterList
                | _ -> None
            getIncompleteLine (Stack<_>()) (line |> Seq.toList)

        data
        |> Seq.choose getIncompleteLine
        |> Seq.map (fun stack ->
            let scores =
                stack
                |> Seq.map (function
                    | '(' -> 1
                    | '[' -> 2
                    | '{' -> 3
                    | '<' -> 4
                    | _ -> 0)

            (0L, scores) ||> Seq.fold (fun i j -> i * 5L + (int64 j))

        )
        |> Seq.sort
        |> Seq.toArray
        |> fun array -> array[array.Length / 2]

module Answers =
    open Data
    open Code

    let ``Example Part 1`` = part1 example
    let ``Puzzle Part 1`` = part1 puzzle
    let ``Example Part 2`` = part2 example
    let ``Puzzle Part 2`` = part2 puzzle

    do
        printfn $"{nameof(``Example Part 1``)}: {``Example Part 1``}"
        printfn $"{nameof(``Puzzle Part 1``)}: {``Puzzle Part 1``}"
        printfn $"{nameof(``Example Part 2``)}: {``Example Part 2``}"
        printfn $"{nameof(``Puzzle Part 2``)}: {``Puzzle Part 2``}"

