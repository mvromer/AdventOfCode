module Day02.PuzzleA

open System
open System.IO
open System.Text.RegularExpressions

type private Entry =
    {
        MinOccur: int
        MaxOccur: int
        RequiredChar: char
        Password: string
    }

    member this.IsPasswordValid() =
        let requiredCharCount =
            this.Password.ToCharArray()
            |> Array.filter (fun c -> c = this.RequiredChar)
            |> Array.length

        requiredCharCount >= this.MinOccur && requiredCharCount <= this.MaxOccur

let private makeEntryFromCaptureGroups (groups: GroupCollection) =
    {
        MinOccur = Int32.Parse(groups.["minOccur"].Value)
        MaxOccur = Int32.Parse(groups.["maxOccur"].Value)
        RequiredChar = groups.["requiredChar"].Value.[0]
        Password = groups.["password"].Value
    }

let private (|Entry|_|) input =
    let pattern = @"^(?<minOccur>\d+)-(?<maxOccur>\d+) (?<requiredChar>[a-z]): (?<password>[a-z]+)$"
    let result = Regex.Match(input, pattern)
    if result.Success
        then Some (makeEntryFromCaptureGroups result.Groups)
        else None

let private hasValidPassword entry =
    match entry with
    | Entry entry -> entry.IsPasswordValid()
    | _ -> failwithf "Could not parse input %s" entry

let main inputFileName =
    let result =
        File.ReadLines inputFileName
        |> Seq.filter hasValidPassword
        |> Seq.length

    printfn "Number valid passwords: %d" result
