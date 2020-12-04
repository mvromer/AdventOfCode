module Program

[<EntryPoint>]
let main argv =
    let puzzle = argv.[0]
    let inputFileName = argv.[1]
    let puzzleMain =
        match puzzle with
        | "1a" -> Day01.PuzzleA.main
        | _ -> failwithf "Unknown puzzle %s" puzzle

    puzzleMain inputFileName
    0
