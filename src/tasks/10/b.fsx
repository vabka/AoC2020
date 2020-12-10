open System
open System.IO

let adapters =
    File.ReadAllLines "input.txt"
    |> Seq.map Int32.Parse

let deviceAdapterRating = adapters |> Seq.max |> (+) 3

let longestChain =
    0
    :: (adapters |> Seq.sort |> Seq.toList)
    @ [ deviceAdapterRating ]

let diffSeq =
    seq {
        for wnd in longestChain |> List.windowed 2 do
            yield
                match wnd with
                | [ current; next ] -> next - current
                | _ -> failwith "Unreachable"
    }

let incrementLast (list: int list) =
    match list with
    | [] -> [1]
    | head :: least -> (head + 1) :: least

let folder state next =
    match next with
    | 1 -> incrementLast state
    | _ -> 0::state

diffSeq
|> Seq.fold folder []
|> Seq.filter (fun x -> x > 0)
|> Seq.map (fun x -> x - 1)
|> Seq.map float
|> Seq.map (fun x -> Math.Pow(2.0, x))
|> Seq.reduce ( * )
|> printfn "%f"

