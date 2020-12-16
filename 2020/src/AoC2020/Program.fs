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
        | "3a" -> Day03.PuzzleA.main
        | "3b" -> Day03.PuzzleB.main
        | "4a" -> Day04.PuzzleA.main
        | "4b" -> Day04.PuzzleB.main
        | "5a" -> Day05.PuzzleA.main
        | "5b" -> Day05.PuzzleB.main
        | "6a" -> Day06.PuzzleA.main
        | "6b" -> Day06.PuzzleB.main
        | "7a" -> Day07.PuzzleA.main
        | "7b" -> Day07.PuzzleB.main
        | "8a" -> Day08.PuzzleA.main
        | "8b" -> Day08.PuzzleB.main
        | "9a" -> Day09.PuzzleA.main
        | "9b" -> Day09.PuzzleB.main
        | _ -> failwithf "Unknown puzzle %s" puzzle

    puzzleMain inputFileName
    0
