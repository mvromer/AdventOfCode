module Day06.PuzzleB

open System.IO

type private ParseState =
    { CurrentGroup: Set<char> list
      Groups: Set<char> list }

    static member Default = { CurrentGroup = []; Groups = [] }

let private parseCustomsDeclarations lines =
    let handleIndividualResponses parseState responses =
        match responses with
        | "" ->
            { CurrentGroup = []
              Groups = (parseState.CurrentGroup |> Set.intersectMany) :: parseState.Groups }
        | _ ->
            { parseState with
                CurrentGroup = (responses.ToCharArray() |> Set.ofArray) :: parseState.CurrentGroup }

    let result = Seq.fold handleIndividualResponses ParseState.Default lines
    if result.CurrentGroup.IsEmpty
        then result.Groups
        else (result.CurrentGroup |> Set.intersectMany) :: result.Groups

let main inputFileName =
    let sumOfCounts =
        File.ReadLines inputFileName
        |> parseCustomsDeclarations
        |> List.map Set.count
        |> List.reduce ( + )

    printfn "Sum of 'yes' responses across all groups: %d" sumOfCounts
