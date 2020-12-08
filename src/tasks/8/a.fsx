#load "../common.fsx"

open Common
open System
open System.Collections.Generic
open System.IO



type Instruction =
    | Nop
    | Acc of int
    | Jmp of int

let parseCmd (line: string) =
    let cmd = line.[0..2]
    let number = line.[4..] |> Int32.Parse
    match cmd with
    | "nop" -> Nop
    | "acc" -> Acc(number)
    | "jmp" -> Jmp(number)
    | _ -> failwith "Unreachable"

let execute (cmds: Instruction []) =
    let set = HashSet()
    let mutable idx = 0
    let mutable acc = 0
    let mutable Break = false
    while not Break do
        let exists = idx |> set.Add |> not
        printfn "d: %i" set.Count
        if exists then
            Break <- true
        else
            let cmd = cmds.[idx]
            idx <-
                match cmd with
                | Nop -> idx + 1
                | Jmp (offset) -> idx + offset
                | Acc (value) ->
                    acc <- acc + value
                    idx + 1
    acc

File.ReadAllText "input.txt"
|> splitByEndl
|> Array.map parseCmd
|> execute
|> printfn "%i"
