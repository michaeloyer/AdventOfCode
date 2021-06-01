#load "../../PuzzleFile.fsx"
#load "../../Utils.fsx"
#r "nuget: FSharp.Text.RegexProvider"

open System.Text.RegularExpressions
open FSharp.Text.RegexProvider

let example = PuzzleFile.text 2020 4 "example"
let puzzle = PuzzleFile.text 2020 4 "puzzle"

let part1 passportText =
    let isValidPassport set =
        [ "byr"
          "iyr"
          "eyr"
          "hgt"
          "hcl"
          "ecl"
          "pid" ]
        |> List.forall (fun item -> set |> Set.contains item)

    let getPassport text =
        Regex.Matches(text, "(\w+):")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Groups.[1].Value)
        |> Set.ofSeq

    let getPassports text =
        text
        |> String.split "\n\n"
        |> Seq.map getPassport

    getPassports passportText
    |> Seq.filter isValidPassport
    |> Seq.length

let ``Part 1 example answer`` = part1 example
let ``Part 1 puzzle answer`` = part1 puzzle

type Height = | Inches of int | Centimeters of int | Invalid
type HeightReader = Regex< "(?<Number>\d+)(?<Unit>in|cm)" >
let getHeight text =
    let height = HeightReader().TypedMatch(text)
    match height.Number.Value, height.Unit.Value with
    | (h, "cm") -> int h |> Centimeters
    | (h, "in") -> int h |> Inches
    | _ -> Invalid

type Passport = {
    BirthYear: int
    IssueYear: int
    ExpirationYear: int
    Height: Height
    HairColor: string
    EyeColor: string
    PassportId: string
}

let part2 passportText =
    let isValidPassport (passport:Passport) =
        (passport.BirthYear |> between 1920 2002) &&
        (passport.IssueYear |> between 2010 2020) &&
        (passport.ExpirationYear |> between 2020 2030) &&
        (match passport.Height with
                | Inches i when i |> between 59 76 -> true
                | Centimeters c when c |> between 150 193 -> true
                | _ -> false ) &&
        (Regex.IsMatch(passport.HairColor, "^#[0-9a-fA-F]{6}$")) &&
        (match passport.EyeColor with
         | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
         | _ -> false) &&
        (Regex.IsMatch(passport.PassportId, "^\d{9}$"))

    let getPassport text =
        let traits =
            Regex.Matches(text, "(\w+):(\S+)")
            |> Seq.cast<Match>
            |> Seq.map (fun m -> m.Groups.[1].Value, m.Groups.[2].Value)
            |> Seq.distinctBy fst
            |> dict

        let traitOrDefault key defaultValue =
            match traits.TryGetValue(key) with
            | (true, v) -> v
            | (false, _) -> defaultValue

        { BirthYear = traitOrDefault "byr" "0" |> int
          IssueYear = traitOrDefault "iyr" "0" |> int
          ExpirationYear = traitOrDefault "eyr" "0" |> int
          Height = traitOrDefault "hgt" "" |> getHeight
          HairColor = traitOrDefault "hcl" ""
          EyeColor = traitOrDefault "ecl" ""
          PassportId = traitOrDefault "pid" "" }

    let getPassports passportText =
        passportText
        |> String.split "\n\n"
        |> Seq.map getPassport

    getPassports passportText
    |> Seq.filter isValidPassport
    |> Seq.length

let ``Part 2 example answer`` = part2 example
let ``Part 2 puzzle answer`` = part2 puzzle
