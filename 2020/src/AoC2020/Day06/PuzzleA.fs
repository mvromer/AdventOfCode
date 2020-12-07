module Day06.PuzzleA

open System
open System.IO
open System.Text.RegularExpressions

type private ParseState =
    { CurrentGroup: Set<char>
      Groups: Set<char> list }

    static member Default = { CurrentGroup = Set.empty; Groups = [] }

let private parseCustomsDeclarations lines =
    let handleIndividualResponses parseState responses =
        match responses with
        | "" ->
            { CurrentGroup = Set.empty
              Groups = parseState.CurrentGroup :: parseState.Groups }
        | _ ->
            { parseState with
                CurrentGroup =
                    responses.ToCharArray()
                    |> Set.ofArray
                    |> Set.union parseState.CurrentGroup }

    let result = Seq.fold handleIndividualResponses ParseState.Default lines
    if result.CurrentGroup |> Set.isEmpty
        then result.Groups
        else result.CurrentGroup :: result.Groups

let main inputFileName =
    let sumOfCounts =
        File.ReadLines inputFileName
        |> parseCustomsDeclarations
        |> List.map Set.count
        |> List.reduce ( + )

    printfn "Sum of 'yes' responses across all groups: %d" sumOfCounts
