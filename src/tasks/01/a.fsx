let input = [ 1721; 979; 366; 299; 675; 1456 ]
let rec duplet input: (int * int) list =
    let first = input |> List.tryHead
    let least = input |> List.tail
    match (first, least) with
    | (Some (n), least) when least.Length > 0 ->
        least
        |> List.map (fun x -> (n, x))
        |> List.append (least |> duplet)
    | _ -> []

input
|> duplet
|> List.filter (fun (a, b) -> a + b = 2020)
|> List.map (fun (a, b) -> a * b)
|> List.iter (printfn "%d")