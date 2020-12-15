#load "../common.fsx"
open Common
open System
open System.IO

//Копипаста из первого дня
let rec duplet input: ('a * 'a) list =
    let first = input |> List.tryHead
    let least = input |> List.tail
    match (first, least) with
    | (Some (n), least) when least.Length > 0 ->
        least
        |> List.map (fun x -> (n, x))
        |> List.append (least |> duplet)
    | _ -> []

let isSumOfTwo (l: int64 list) n =
    l
    |> duplet
    |> Seq.map (fun (a, b) -> a + b)
    |> Seq.contains n

let numbers =
    File.ReadAllText "input.txt"
    |> splitByEndl
    |> Seq.map Int64.Parse
    |> Seq.toList

let map i n =
    if i < 25 then
        None
    else
        let prev = numbers.[i - 25..i]
        let s = isSumOfTwo prev n
        Some(i, n, s)

let (_, n, _) =
    numbers
    |> Seq.mapi map
    |> Seq.filter isSome
    |> Seq.map unwrap
    |> Seq.filter (fun (i, n, s) -> (not s))
    |> Seq.head

printfn "%i" n
