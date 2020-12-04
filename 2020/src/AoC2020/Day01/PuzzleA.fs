module Day01.PuzzleA

open System
open System.IO

let rec findPairWithSum targetSum numbers =
    let findHeadAndTailElementWithSum targetSum head tail =
        let tryUpdateResult currentResult currentTailElement =
            match currentResult with
            | None -> if head + currentTailElement = targetSum then Some (head, currentTailElement) else None
            | _ -> currentResult

        List.fold tryUpdateResult None tail

    match numbers with
    | x :: xs ->
        let result = findHeadAndTailElementWithSum targetSum x xs
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
    | Some (x, y) -> printfn "Found pair summing to target %d: %d + %d = %d // %d * %d = %d" targetSum x y (x + y) x y (x * y)
    | None -> failwithf "Did not find a pair the sums to the target %d" targetSum
