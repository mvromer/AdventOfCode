module Program

[<EntryPoint>]
let main argv =
    let puzzle = argv.[0]
    let inputFileName = argv.[1]
    let puzzleMain =
        match puzzle with
        | "1a" -> Day01.PuzzleA.main
        | "1b" -> Day01.PuzzleB.main
        | "2a" -> Day02.PuzzleA.main
        | "2b" -> Day02.PuzzleB.main
        | _ -> failwithf "Unknown puzzle %s" puzzle

    puzzleMain inputFileName
    0
