module Day09.PuzzleA

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO

type private Entry =
    { Augend: int64
      PairwiseSums: int64 list }

let private initializeWindow preamble =
    let computePairwiseSums augend rest = rest |> List.map (( + ) augend)

    let rec foldPreamble (currentQueue: ImmutableQueue<Entry>) numbers =
        match numbers with
        | augend :: rest ->
            let entry = { Augend = augend; PairwiseSums = computePairwiseSums augend rest }
            foldPreamble (currentQueue.Enqueue (entry)) rest
        | [] -> currentQueue

    foldPreamble ImmutableQueue<Entry>.Empty preamble

let private violatesXmas window value =
    not (window |> Seq.exists (fun entry -> List.contains value entry.PairwiseSums))

let private updateWindow window newValue =
    let updateEntry entry newValue = { entry with PairwiseSums = (entry.Augend + newValue) :: entry.PairwiseSums }
    let shiftedWindow =
        window
        |> Seq.skip 1
        |> Seq.fold
            (fun (queue: ImmutableQueue<Entry>) entry ->
                queue.Enqueue (updateEntry entry newValue))
            ImmutableQueue<Entry>.Empty
    shiftedWindow.Enqueue ({ Augend = newValue; PairwiseSums = []})

let rec private tryFindXmasViolation (numbers: IEnumerator<int64>) window = seq {
    match numbers.MoveNext() with
    | true ->
        match numbers.Current with
        | currentValue when violatesXmas window currentValue -> Some currentValue
        | currentValue -> yield! tryFindXmasViolation numbers (updateWindow window currentValue)
    | false -> None
}

let main inputFileName =
    let windowSize = 25
    let numbers = File.ReadLines inputFileName
    let window =
        numbers
        |> Seq.take windowSize
        |> Seq.map Int64.Parse
        |> List.ofSeq
        |> initializeWindow

    let remainingNumbers =
        numbers
        |> Seq.skip windowSize
        |> Seq.map Int64.Parse

    use remainingEnumerator = remainingNumbers.GetEnumerator()
    let result =
        tryFindXmasViolation remainingEnumerator window
        |> Seq.exactlyOne
        |> function
            | Some violation -> violation
            | None -> failwith "Failed to find any XMAS violation"

    printfn "XMAS violation: %d" result
