#load "../common.fsx"

open Common
open System
open System.Collections.Generic
open System.IO

type Instruction =
    | Nop of int
    | Acc of int
    | Jmp of int

let parseCmd (line: string) =
    let cmd = line.[0..2]
    let number = line.[4..] |> Int32.Parse
    match cmd with
    | "nop" -> Nop(number)
    | "acc" -> Acc(number)
    | "jmp" -> Jmp(number)
    | _ -> failwith "Unreachable"

let execute (cmds: Instruction list) =
    let set = HashSet()
    let mutable idx = 0
    let mutable acc = 0
    let mutable halt = false
    while (not halt) && idx < cmds.Length do
        if idx |> set.Add |> not then
            halt <- true
        else
            let cmd = cmds.[idx]
            idx <-
                match cmd with
                | Nop (_) -> idx + 1
                | Jmp (offset) -> idx + offset
                | Acc (value) ->
                    acc <- acc + value
                    idx + 1
    printfn "state:\tacc=%i;\tidx=%i;\thalt=%b" acc idx halt
    if not halt then Some(acc) else None

let tryFix (cmds: Instruction list) =
    seq {
        for i in [ 0 .. cmds.Length ] do
            printf "%i:\t" (i + 1)

            let modifiedCode =
                match cmds.[i] with
                | Acc (_) -> None
                | Nop (0) -> None
                | Jmp (1) -> None
                | Nop (x) -> cmds |> withNth i (Jmp(x)) |> Some
                | Jmp (x) -> cmds |> withNth i (Nop(x)) |> Some

            if modifiedCode.IsSome then
                printfn "check"
                modifiedCode.Value
            else
                printfn "skip"
    }

File.ReadAllText "input.txt"
|> splitByEndl
|> Seq.map parseCmd
|> Seq.toList
|> tryFix
|> Seq.map execute
|> firstSome
|> printfn "result: %i"
