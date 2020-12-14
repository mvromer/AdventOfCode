module Day08.PuzzleB

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
    | Nop of operand : int

type private LineNumber = int
type private AnnotatedInstruction = AnnotatedInstruction of LineNumber * Instruction
type private ActiveMutation = ActiveMutation of replacementInstruction : AnnotatedInstruction

type private DebuggerState =
    { Instructions: AnnotatedInstruction[]
      ProgramCounter: LineNumber
      Accumulator: int
      ActiveMutation: ActiveMutation option
      VisitedLines: Set<LineNumber> }

    static member Default = {
        Instructions = Array.empty
        ProgramCounter = 0
        Accumulator = 0
        ActiveMutation = None
        VisitedLines = Set.empty
    }

let private parseInstruction line =
    match line with
    | ParseInstruction @"^acc ([+\-]\d+)$" [ addend ]-> Acc (Int32.Parse addend)
    | ParseInstruction @"^jmp ([+\-]\d+)$" [ offset ] -> Jmp (Int32.Parse offset)
    | ParseInstruction @"^nop ([+\-]\d+)$" [ operand ] -> Nop (Int32.Parse operand)
    | _ -> failwithf "Invaild instruction %s" line

let private buildMutations instructions =
    let buildMutation =
        function
        | AnnotatedInstruction (line, Nop operand) -> Some (AnnotatedInstruction (line, Jmp operand))
        | AnnotatedInstruction (line, Jmp offset) -> Some (AnnotatedInstruction (line, Nop offset))
        | _ -> None

    instructions |> Array.choose buildMutation

type private ExecutionState =
    | Running
    | InfiniteLoop
    | Terminated of finalState : DebuggerState

let private getExecutionState debuggerState =
    match debuggerState.ProgramCounter with
    | endOfProgram when endOfProgram = debuggerState.Instructions.Length -> Terminated debuggerState
    | repeatLine when debuggerState.VisitedLines |> Set.contains repeatLine -> InfiniteLoop
    | _ -> Running

let private getCurrentInstruction debuggerState =
    let currentInstruction = debuggerState.Instructions.[debuggerState.ProgramCounter]
    match debuggerState.ActiveMutation with
    | Some (ActiveMutation (AnnotatedInstruction (lineToMutate, _) as mutatedInstruction))
        when lineToMutate = debuggerState.ProgramCounter -> mutatedInstruction
    | _ -> currentInstruction

let rec private runProgram debuggerState =
    match debuggerState |> getExecutionState with
    | InfiniteLoop as result -> result
    | Terminated _ as result -> result
    | Running ->
        let (AnnotatedInstruction (line, instruction)) = debuggerState |> getCurrentInstruction
        let newDebuggerState =
            match instruction with
            | Acc addend ->
                { debuggerState with
                    Accumulator = debuggerState.Accumulator + addend
                    ProgramCounter = debuggerState.ProgramCounter + 1 }
            | Jmp offset -> { debuggerState with ProgramCounter = debuggerState.ProgramCounter + offset }
            | Nop _ -> { debuggerState with ProgramCounter = debuggerState.ProgramCounter + 1 }

        let newVisitedLines = Set.add line newDebuggerState.VisitedLines
        { newDebuggerState with VisitedLines = newVisitedLines } |> runProgram

let main inputFileName =
    let instructions =
        File.ReadLines inputFileName
        |> Seq.mapi (fun idx line -> AnnotatedInstruction (idx, parseInstruction line))
        |> Array.ofSeq

    let initialState = { DebuggerState.Default with Instructions = instructions }
    let mutationResults = seq {
        for mutatedInstruction in (instructions |> buildMutations) ->
            { initialState with ActiveMutation = Some (ActiveMutation mutatedInstruction) } |> runProgram
    }

    mutationResults
    |> Seq.tryFind (function | Terminated _ -> true | _ -> false)
    |> function
        | Some (Terminated finalState) -> printfn "Accumulator at terminated state: %d" finalState.Accumulator
        | _ -> failwithf "Failed to find a mutation that can yield a terminated state."
