module Day01.PuzzleA

open System
open System.IO

let private tryFindPairWithSum targetSum firstNumber numbers =
    let tryUpdateResult currentResult currentNumber =
        match currentResult with
        | None ->
            if firstNumber + currentNumber = targetSum
            then Some (firstNumber, currentNumber)
            else None
        | _ -> currentResult

    List.fold tryUpdateResult None numbers

let rec private findPairWithSum targetSum numbers =
    match numbers with
    | x :: xs ->
        let result = tryFindPairWithSum targetSum x xs
        match result with
        | None -> findPairWithSum targetSum xs
        | _ -> result
    | [] -> None

let main inputFileName =
    let targetSum = 2020
    let result =
        File.ReadAllLines inputFileName
        |> Array.map Int32.Parse
        |> List.ofArray
        |> findPairWithSum targetSum

    match result with
    | Some (x, y) ->
        printfn "Found pair summing to target %d: %d + %d = %d" targetSum x y (x + y)
        printfn "Product of pair: %d" (x * y)
    | None -> failwithf "Did not find a pair the sums to the target %d" targetSum
