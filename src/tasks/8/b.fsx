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
    let mutable Break = false
    while (not Break) && idx < cmds.Length do
        let exists = idx |> set.Add |> not
        printfn "d: %i" set.Count
        if exists then
            Break <- true
        else
            let cmd = cmds.[idx]
            idx <-
                match cmd with
                | Nop (_) -> idx + 1
                | Jmp (offset) -> idx + offset
                | Acc (value) ->
                    acc <- acc + value
                    idx + 1
    if idx >= cmds.Length then Some(acc) else None

let tryFix (cmds: Instruction list) = 
    seq {
        for i in [0..cmds.Length] do
            let cmd = cmds.[i]
            let swapped = match cmd with 
                          | Nop(x) -> Jmp(x)
                          | Jmp(x) -> Nop(x) 
                          | Acc(x) -> Acc(x)
            let code = match swapped with 
                       | Acc(_) -> None
                       | x -> cmds |> List.mapi (fun idx v -> if i = idx then x else v) |> Some
            if code.IsSome then
                printfn "l: %i" i
                yield code.Value
            
    }

File.ReadAllText "input.txt"
|> splitByEndl
|> Array.map parseCmd
|> Array.toList
|> tryFix
|> Seq.map execute
|> Seq.filter isSome
|> Seq.map getVal
|> Seq.head
|> printfn "%i"