module Program

[<EntryPoint>]
let main argv =
    let puzzle = argv.[0]
    let inputFileName = argv.[1]

    match puzzle with
    | "1a" -> Day01.PuzzleA.main inputFileName
    | _ -> failwithf "Unknown puzzle %s" puzzle

    0
