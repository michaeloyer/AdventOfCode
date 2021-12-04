module Data =
    open System.IO
    let private readData name =
        Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt")
        |> File.ReadAllLines
    let example = readData "example"
    let puzzle = readData "puzzle"

module Code =

    let part1 data =
        let bitCounts =
            ((Array.create (data |> Array.head |> String.length) 0), data)
            ||> Array.fold (fun ns datum ->
                Array.zip ns (datum.ToCharArray())
                |> Array.map (fun (n, char) -> n + if char = '1' then 1 else - 1)
            )

        let readBinary bits =
            bits
            |> Seq.rev
            |> Seq.mapi (fun i bit -> bit <<< i)
            |> Seq.sum

        let gammaRate =
            bitCounts
            |> Seq.map (fun n -> if n > 0 then 1 else 0)
            |> readBinary

        let epsilonRate =
            bitCounts
            |> Seq.map (fun n -> if n > 0 then 0 else 1)
            |> readBinary

        gammaRate * epsilonRate

    let part2 data =
        let getRate getTargetBit data =
            let getBitCount index (data: string seq) =
                data |> Seq.sumBy (fun datum -> if datum[index] = '1' then 1 else -1)

            let rec getRate i (data: string list) =
                printfn "%A" data
                match data with
                | [] -> "0"
                | [ datum ] -> datum
                | data ->
                    let targetBit = getBitCount i data |> getTargetBit
                    let data =
                        [ for item in data do
                            if item[i] = targetBit then
                                yield item ]
                    getRate (i + 1) data

            let bits = getRate 0 (data |> List.ofArray)
            System.Convert.ToInt32(bits, 2)

        let oxygenRating =
            getRate (fun i -> if i >= 0 then '1' else '0') data

        let co2Rating =
            getRate (fun i -> if i < 0 then '1' else '0') data

        oxygenRating * co2Rating


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

