module Day04.PuzzleB

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
    let result = Regex.Match(input, "^byr:(?<value>\d{4})$")
    if result.Success then
        let birthYear = Int32.Parse(result.Groups.["value"].Value)
        if 1920 <= birthYear && birthYear <= 2002 then Some birthYear else None
    else
        None

let private (|IssueYear|_|) input =
    let result = Regex.Match(input, "^iyr:(?<value>\d{4})$")
    if result.Success then
        let issueYear = Int32.Parse(result.Groups.["value"].Value)
        if 2010 <= issueYear && issueYear <= 2020 then Some issueYear else None
    else
        None

let private (|ExpirationYear|_|) input =
    let result = Regex.Match(input, "^eyr:(?<value>\d{4})$")
    if result.Success then
        let expirationYear = Int32.Parse(result.Groups.["value"].Value)
        if 2020 <= expirationYear && expirationYear <= 2030 then Some expirationYear else None
    else
        None

let private (|Height|_|) input =
    let result = Regex.Match(input, "^hgt:(?<value>\d+)(?<units>cm|in)$")
    if result.Success then
        let height = Int32.Parse(result.Groups.["value"].Value)
        let units = result.Groups.["units"].Value
        match units with
        | "in" -> if 59 <= height && height <= 76 then Some (height, units) else None
        | "cm" -> if 150 <= height && height <= 193 then Some (height, units) else None
        | _ -> None
    else
        None

let private (|HairColor|_|) input =
    let result = Regex.Match(input, "^hcl:(?<value>#[0-9a-f]{6})$")
    if result.Success then Some result.Groups.["value"].Value else None

let private (|EyeColor|_|) input =
    let result = Regex.Match(input, "^ecl:(?<value>amb|blu|brn|gry|grn|hzl|oth)$")
    if result.Success then Some result.Groups.["value"].Value else None

let private (|PassportId|_|) input =
    let result = Regex.Match(input, "^pid:(?<value>[0-9]{9})$")
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
