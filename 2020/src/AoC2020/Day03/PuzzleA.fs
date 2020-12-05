module Day03.PuzzleA

open System.IO

type TreeFoldState =
    {
        CurrentIndex: int
        NumberTrees: int
    }

    static member Defaut = { CurrentIndex = 0; NumberTrees = 0; }

let checkForTree currentState (currentLine: string) =
    let treeHit = currentLine.[currentState.CurrentIndex] = '#'
    {
        CurrentIndex = (currentState.CurrentIndex + 3) % currentLine.Length
        NumberTrees = currentState.NumberTrees + (if treeHit then 1 else 0)
    }

let main inputFileName =
    let result =
        File.ReadLines inputFileName
        |> Seq.fold checkForTree TreeFoldState.Defaut

    printfn "Number trees: %d" result.NumberTrees
