module Day10.PuzzleA

open System
open System.IO

let main inputFileName =
    let adapterJolts = File.ReadLines inputFileName |> Seq.map Int32.Parse
    let distroProduct =
        [(Seq.singleton 0); adapterJolts; (Seq.singleton ((Seq.max adapterJolts) + 3))]
        |> Seq.concat
        |> Seq.sort
        |> Seq.pairwise
        |> Seq.map (fun (x, y) -> y - x)
        |> Seq.countBy id
        |> Seq.choose
            (function
                | (diff, count) when diff = 1 || diff = 3 -> Some count
                | _ -> None)
        |> Seq.reduce ( * )

    printfn "Product of 1-jolt and 3-jolt differences: %d" distroProduct
