open System
open System.IO

let adapters =
    File.ReadAllLines "input.txt"
    |> Seq.map Int32.Parse

let deviceAdapterRating = adapters |> Seq.max |> (+) 3

let fullChain =
    deviceAdapterRating
    :: (adapters |> Seq.sortDescending |> Seq.toList) @ [0]

let result =
    let mutable diff3Count = 0
    let mutable diff1Count = 0
    for wnd in fullChain |> List.windowed 2 do
        match wnd with
        | [current; next] when current - next = 1 -> diff1Count <- diff1Count + 1
        | [current; next] when current - next = 3 -> diff3Count <- diff3Count + 1
        | x -> printfn "%A" x
    diff1Count * diff3Count

printfn "%i" result