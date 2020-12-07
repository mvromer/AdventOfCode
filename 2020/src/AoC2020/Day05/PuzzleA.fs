module Day05.PuzzleA

open System.IO
open System.Text.RegularExpressions

type private SearchState = { LowerBound: int; UpperBound: int; NextBoundShift: int }

let private computePosition minPosition maxPosition lowerHalfPositionCode upperHalfPositionCode (encodedPosition: string) =
    let handlePositionCode searchState code =
        match code with
        | lowerCode when lowerCode = lowerHalfPositionCode ->
            { searchState with
                UpperBound = searchState.UpperBound - searchState.NextBoundShift
                NextBoundShift = searchState.NextBoundShift / 2 }
        | upperCode when upperCode = upperHalfPositionCode ->
            { searchState with
                LowerBound = searchState.LowerBound + searchState.NextBoundShift
                NextBoundShift = searchState.NextBoundShift / 2 }
        | _ -> failwithf "Invalid position code %c found" code

    let initialState = {
        LowerBound = minPosition
        UpperBound = maxPosition
        NextBoundShift = (maxPosition - minPosition + 1) / 2
    }

    let searchState = encodedPosition.ToCharArray() |> Array.fold handlePositionCode initialState
    assert (searchState.LowerBound = searchState.UpperBound)
    searchState.LowerBound

let private computeSeatId boardingPass =
    let result = Regex.Match(boardingPass, "^(?<row>[FB]{7})(?<column>[LR]{3})$")
    if result.Success then
        let row = computePosition 0 127 'F' 'B' result.Groups.["row"].Value
        let column = computePosition 0 7 'L' 'R' result.Groups.["column"].Value
        row * 8 + column
    else
        failwithf "Invalid boarding pass %s" boardingPass

let main inputFileName =
    let maxSeatId =
        File.ReadLines inputFileName
        |> Seq.map computeSeatId
        |> Seq.max

    printfn "Max seat ID: %d" maxSeatId
