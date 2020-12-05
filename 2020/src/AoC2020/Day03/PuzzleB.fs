module Day03.PuzzleB

open System.IO

type private Increment = { Line: int; Index: int }

type private TreeFoldState =
    {
        CurrentIndex: int
        NumberTrees: int
    }

    static member Default = { CurrentIndex = 0; NumberTrees = 0; }

let private checkForTree indexIncrement currentState (currentLine: string) =
    let treeHit = currentLine.[currentState.CurrentIndex] = '#'
    {
        CurrentIndex = (currentState.CurrentIndex + indexIncrement) % currentLine.Length
        NumberTrees = currentState.NumberTrees + (if treeHit then 1 else 0)
    }

let private countTrees lines increment =
    let checkForTree = checkForTree increment.Index
    let result =
        lines
        |> Seq.mapi (fun idx line -> (idx, line))
        |> Seq.filter (fun (idx, _) -> idx % increment.Line = 0)
        |> Seq.map snd
        |> Seq.fold checkForTree TreeFoldState.Default
    result.NumberTrees

let main inputFileName =
    let lines = File.ReadLines inputFileName
    let increments = [
        { Line = 1; Index = 1 }
        { Line = 1; Index = 3 }
        { Line = 1; Index = 5 }
        { Line = 1; Index = 7 }
        { Line = 2; Index = 1 }
    ]
    let result =
        increments
        |> List.map (countTrees lines)
        |> List.reduce ( * )

    printfn "Tree product: %d" result
