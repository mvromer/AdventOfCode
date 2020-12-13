module Day08.PuzzleA

open System
open System.IO
open System.Text.RegularExpressions

let private (|ParseInstruction|_|) pattern instruction =
    let m = Regex.Match(instruction, pattern)
    match m.Success with
    | true -> Some (List.tail [ for x in m.Groups -> x.Value ])
    | false -> None

type private Instruction =
    | Acc of addend : int
    | Jmp of offset : int
    | Nop

type private LineNumber = int
type private AnnotatedInstruction = AnnotatedInstruction of LineNumber * Instruction

type private DebuggerState =
    { Instructions: AnnotatedInstruction[]
      ProgramCounter: LineNumber
      Accumulator: int
      VisitedLines: Set<LineNumber> }

    static member Default = {
        Instructions = Array.empty
        ProgramCounter = 0
        Accumulator = 0
        VisitedLines = Set.empty
    }

let private parseInstruction line =
    match line with
    | ParseInstruction @"^acc ([+\-]\d+)$" [ addend ]-> Acc (Int32.Parse addend)
    | ParseInstruction @"^jmp ([+\-]\d+)$" [ offset ] -> Jmp (Int32.Parse offset)
    | ParseInstruction @"^nop ([+\-]\d+)$" [ _ ] -> Nop
    | _ -> failwithf "Invaild instruction %s" line

let rec private runProgramUntilRepeatInstruction debuggerState =
    match debuggerState.VisitedLines |> Set.contains debuggerState.ProgramCounter with
    | true -> debuggerState
    | false ->
        let { Instructions = instructions; ProgramCounter = programCounter } = debuggerState
        let (AnnotatedInstruction (line, instruction)) = instructions.[programCounter]
        let newDebuggerState =
            match instruction with
            | Acc addend ->
                { debuggerState with
                    Accumulator = debuggerState.Accumulator + addend
                    ProgramCounter = debuggerState.ProgramCounter + 1 }
            | Jmp offset -> { debuggerState with ProgramCounter = debuggerState.ProgramCounter + offset }
            | Nop -> { debuggerState with ProgramCounter = debuggerState.ProgramCounter + 1 }

        let newVisitedLines = Set.add line newDebuggerState.VisitedLines
        { newDebuggerState with VisitedLines = newVisitedLines } |> runProgramUntilRepeatInstruction

let main inputFileName =
    let instructions =
        File.ReadLines inputFileName
        |> Seq.mapi (fun idx line -> AnnotatedInstruction (idx, parseInstruction line))
        |> Array.ofSeq

    let debuggerState =
        { DebuggerState.Default with Instructions = instructions }
        |> runProgramUntilRepeatInstruction

    printfn "Accumulator at point of infinite loop: %d" debuggerState.Accumulator
