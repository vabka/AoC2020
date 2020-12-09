#load "../common.fsx"
open Common
open System
open System.IO

let rec duplet input: ('a * 'a) list =
    let first = input |> List.tryHead
    let least = input |> List.tail
    match (first, least) with
    | (Some (n), least) when least.Length > 0 ->
        least
        |> List.map (fun x -> (n, x))
        |> List.append (least |> duplet)
    | _ -> []

let numbers =
    File.ReadAllText "input.txt"
    |> splitByEndl
    |> Seq.map Int64.Parse
    |> Seq.toList

let n = 257342611L
let all = numbers |> List.mapi (fun i n -> i, n)


for (i, _) in all do
    let mutable count = 1
    let mutable Break = false
    while not Break do
        let range = numbers.[i..i + count]
        let sum = range |> Seq.sum
        if sum = n then
            printfn "%i" ((range |> Seq.min) + (range |> Seq.max))
            Break <- true
        if sum > n
           || sum < n && (i + count) >= numbers.Length then
            Break <- true
        else if sum < n then
            count <- count + 1
            
