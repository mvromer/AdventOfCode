module Day04.PuzzleA

open System
open System.IO
open System.Text.RegularExpressions

type private ParseState =
    { CurrentFragments: string list
      Passports: string list }

    static member Default = { CurrentFragments = []; Passports = [] }

type private ValidatorState =
    { HasBirthYear: bool
      HasIssueYear: bool
      HasExpirationYear: bool
      HasHeight: bool
      HasHairColor: bool
      HasEyeColor: bool
      HasPassportId: bool
      HasCountryId: bool }

    static member Default = {
        HasBirthYear = false
        HasIssueYear = false
        HasExpirationYear = false
        HasHeight = false
        HasHairColor = false
        HasEyeColor = false
        HasPassportId = false
        HasCountryId = false
    }

    member this.IsValid() =
        let requiredFields = [
            this.HasBirthYear
            this.HasIssueYear
            this.HasExpirationYear
            this.HasHeight
            this.HasHairColor
            this.HasEyeColor
            this.HasPassportId
        ]
        List.fold ( && ) true requiredFields

let private (|BirthYear|_|) input =
    let result = Regex.Match(input, "^byr:(?<value>\S+)$")
    if result.Success then Some result.Groups.["value"].Value else None

let private (|IssueYear|_|) input =
    let result = Regex.Match(input, "^iyr:(?<value>\S+)$")
    if result.Success then Some result.Groups.["value"].Value else None

let private (|ExpirationYear|_|) input =
    let result = Regex.Match(input, "^eyr:(?<value>\S+)$")
    if result.Success then Some result.Groups.["value"].Value else None

let private (|Height|_|) input =
    let result = Regex.Match(input, "^hgt:(?<value>\S+)$")
    if result.Success then Some result.Groups.["value"].Value else None

let private (|HairColor|_|) input =
    let result = Regex.Match(input, "^hcl:(?<value>\S+)$")
    if result.Success then Some result.Groups.["value"].Value else None

let private (|EyeColor|_|) input =
    let result = Regex.Match(input, "^ecl:(?<value>\S+)$")
    if result.Success then Some result.Groups.["value"].Value else None

let private (|PassportId|_|) input =
    let result = Regex.Match(input, "^pid:(?<value>\S+)$")
    if result.Success then Some result.Groups.["value"].Value else None

let private (|CountryId|_|) input =
    let result = Regex.Match(input, "^cid:(?<value>\S+)$")
    if result.Success then Some result.Groups.["value"].Value else None

let private parsePassports lines =
    let handleFragment parseState fragment =
        match fragment with
        | "" ->
            { CurrentFragments = []
              Passports = (String.concat " " parseState.CurrentFragments) :: parseState.Passports }
        | _ -> { parseState with CurrentFragments = fragment :: parseState.CurrentFragments }

    let result = Seq.fold handleFragment ParseState.Default lines
    if result.CurrentFragments.IsEmpty
        then result.Passports
        else (String.concat " " result.CurrentFragments) :: result.Passports

let private isPassportValid (passport: string) =
    let validateFragment validateState fragment =
        match fragment with
        | BirthYear _ -> { validateState with HasBirthYear = true }
        | IssueYear _ -> { validateState with HasIssueYear = true }
        | ExpirationYear _ -> { validateState with HasExpirationYear = true }
        | Height _ -> { validateState with HasHeight = true }
        | HairColor _ -> { validateState with HasHairColor = true }
        | EyeColor _ -> { validateState with HasEyeColor = true }
        | PassportId _ -> { validateState with HasPassportId = true }
        | CountryId _ -> { validateState with HasCountryId = true }
        | _ -> validateState

    let result =
        passport.Split " "
        |> Array.fold validateFragment ValidatorState.Default

    result.IsValid()

let main inputFileName =
    let numberValid =
        File.ReadLines inputFileName
        |> parsePassports
        |> List.filter isPassportValid
        |> List.length

    printfn "Number valid passports: %d" numberValid
