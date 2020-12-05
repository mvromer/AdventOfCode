module Day02.PuzzleB

open System
open System.IO
open System.Text.RegularExpressions

type private Entry =
    {
        FirstIndex: int
        SecondIndex: int
        RequiredChar: char
        Password: string
    }

    member this.IsPasswordValid() =
        // For Boolean A, B: (A XOR B) <=> (A != B)
        (this.Password.[this.FirstIndex] = this.RequiredChar) <> (this.Password.[this.SecondIndex] = this.RequiredChar)

let private makeEntryFromCaptureGroups (groups: GroupCollection) =
    {
        // Convert 1-based indices from raw entry to 0-based.
        FirstIndex = Int32.Parse(groups.["firstIndex"].Value) - 1
        SecondIndex = Int32.Parse(groups.["secondIndex"].Value) - 1
        RequiredChar = groups.["requiredChar"].Value.[0]
        Password = groups.["password"].Value
    }

let private (|Entry|_|) input =
    let pattern = @"^(?<firstIndex>\d+)-(?<secondIndex>\d+) (?<requiredChar>[a-z]): (?<password>[a-z]+)$"
    let result = Regex.Match(input, pattern)
    if result.Success
        then Some (makeEntryFromCaptureGroups result.Groups)
        else None

let private isValidPassport entry =
    match entry with
    | Entry entry -> entry.IsPasswordValid()
    | _ -> failwithf "Could not parse input %s" entry

let main inputFileName =
    let result =
        File.ReadLines inputFileName
        |> Seq.filter isValidPassport
        |> Seq.length

    printfn "Number valid passports: %d" result
