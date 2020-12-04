module Day01.PuzzleB

open System
open System.IO

let private tryFindTripleWithSum targetSum firstNumber secondNumber numbers =
    let partialSum = firstNumber + secondNumber
    let tryUpdateResult currentResult currentNumber =
        match currentResult with
        | None ->
            if partialSum + currentNumber = targetSum
            then Some (firstNumber, secondNumber, currentNumber)
            else None
        | _ -> currentResult

    List.fold tryUpdateResult None numbers

let rec private tryFindTripleWithSumGivenFirstNumber targetSum firstNumber numbers =
    match numbers with
    | x :: xs ->
        let result = tryFindTripleWithSum targetSum firstNumber x xs
        match result with
        | None -> tryFindTripleWithSumGivenFirstNumber targetSum firstNumber xs
        | _ -> result
    | [] -> None

let rec private findTripleWithSum targetSum numbers =
    match numbers with
    | x :: xs ->
        let result = tryFindTripleWithSumGivenFirstNumber targetSum x xs
        match result with
        | None -> findTripleWithSum targetSum xs
        | _ -> result
    | [] -> None

let main inputFileName =
    let targetSum = 2020
    let result =
        File.ReadAllLines inputFileName
        |> Array.map Int32.Parse
        |> List.ofArray
        |> findTripleWithSum targetSum

    match result with
    | Some (x, y, z) ->
        printfn "Found triple summing to target %d: %d + %d + %d = %d" targetSum x y z (x + y + z)
        printfn "Product of triple: %d" (x * y * z)
    | None -> failwithf "Did not find a triple that sums to the target %d" targetSum
