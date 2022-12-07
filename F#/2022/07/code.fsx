#r "nuget:FParsec"

type PuzzleInput = { FullText: string; Lines: string list }

module AdventOfCode =
    open FParsec

    type File = {
        Name: string
        Size: int64
    }

    type Directory = {
        Name: string
        Directories: ResizeArray<Directory>
        Parent: Directory option
        Files:  ResizeArray<File>
    }

    let commandParser =
        let changeDirectory =
            skipString "$ cd " >>. choice [
                skipString "/" >>= fun _ -> updateUserState (
                    fun currentDirectory ->
                        let mutable directory = currentDirectory
                        while Option.isSome directory.Parent do
                            directory <- directory.Parent.Value

                        directory
                    )
                many1Chars letter >>=
                    fun name -> updateUserState (
                        fun currentDirectory ->
                            currentDirectory.Directories
                            |> Seq.find (fun directory -> directory.Name = name)
                    )
                skipString ".." >>.
                    updateUserState (
                        fun currentDirectory ->
                            currentDirectory.Parent |> Option.defaultValue currentDirectory
                    )
            ] .>> skipNewline

        let listFiles =
            let directory = attempt (
                skipString "dir " >>. many1Chars letter .>>. getUserState
                |>> fun (name, currentDirectory) -> Choice1Of2 { Name = name; Directories = ResizeArray(); Files = ResizeArray(); Parent = Some currentDirectory}
            )

            let file = attempt (
                pint64 .>> pchar ' ' .>>. many1Chars (letter <|> pchar '.')
                |>> fun (size, name) -> Choice2Of2 { Size = size; Name = name}
            )

            skipString "$ ls" >>. skipNewline
            >>. (sepEndBy (directory <|> file) skipNewline)
            >>= fun (fileSystemObjects) ->
                updateUserState (fun currentDirectory ->
                    currentDirectory.Directories.AddRange [
                            for object in fileSystemObjects do
                                match object with
                                | Choice1Of2 dir -> yield dir
                                | _ -> ()
                        ]
                    currentDirectory.Files.AddRange [
                            for object in fileSystemObjects do
                                match object with
                                | Choice2Of2 file -> yield file
                                | _ -> ()
                        ]
                    currentDirectory)

        many (changeDirectory <|> listFiles)

    let parseFileSystem text =
        let root = { Name = "/"; Directories = ResizeArray(); Parent = None; Files = ResizeArray()}
        match runParserOnString (commandParser .>> eof) root "FileStructure" text with
        | Success (_, directory, _) ->
            let mutable directory = directory
            while Option.isSome directory.Parent do
                directory <- directory.Parent.Value

            directory
        | Failure (message, _, _) -> failwith message

    let rec getDirectorySize directory =
        let directorySize = directory.Files |> Seq.sumBy (fun file -> file.Size)
        let directorySizes = directory.Directories |> Seq.sumBy getDirectorySize

        directorySize + directorySizes

    let rec getDirectories directory = [
        yield directory
        for directory in directory.Directories do
            yield! getDirectories directory
    ]

    let part1 (puzzle:PuzzleInput) =
        parseFileSystem puzzle.FullText
        |> getDirectories
        |> List.map getDirectorySize
        |> List.filter (fun size -> size <= 100000)
        |> List.sum


    let part2 (puzzle:PuzzleInput) =
        let sizes =
            parseFileSystem puzzle.FullText
            |> getDirectories
            |> List.map getDirectorySize

        let usedSpace = List.max sizes
        let unusedSpace = 70_000_000L - usedSpace

        sizes
        |> List.filter (fun size -> unusedSpace + size >= 30_000_000L)
        |> List.min

module Input =
    open System.IO
    let private readData name =
        let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, $"{name}.txt"))
        { Lines = List.ofArray lines
          FullText = String.concat "\n" lines }

    let rec example = readData (nameof example)
    let rec puzzle = readData (nameof puzzle)

module Output =
    type PuzzleInputOutput = { LineCount: int }
    fsi.AddPrintTransformer(fun (input:PuzzleInput) -> { LineCount = input.Lines.Length })

    let rec ``Example Part 1`` = let answer = AdventOfCode.part1 Input.example in printfn $"{nameof ``Example Part 1``}: {answer}"; answer
    let rec ``Puzzle Part 1`` = let answer = AdventOfCode.part1 Input.puzzle in printfn $"{nameof ``Puzzle Part 1``}: {answer}"; answer
    let rec ``Example Part 2`` = let answer = AdventOfCode.part2 Input.example in printfn $"{nameof ``Example Part 2``}: {answer}"; answer
    let rec ``Puzzle Part 2`` = let answer = AdventOfCode.part2 Input.puzzle in printfn $"{nameof ``Puzzle Part 2``}: {answer}"; answer
