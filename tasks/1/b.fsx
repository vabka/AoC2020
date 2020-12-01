let input = [ 1721; 979; 366; 299; 675; 1456 ]

let rec duplet (input: 'a list): ('a * 'a) list =
    let first = input |> List.tryHead
    let least = input |> List.tail
    match (first, least) with
    | (Some (n), least) when least.Length > 0 ->
        least
        |> List.map (fun x -> (n, x))
        |> List.append (least |> duplet)
    | _ -> []

let rec triplet (input: 'a list): ('a * 'a * 'a) list =
    let first = input |> List.tryHead
    let least = input |> List.tail
    match (first, least) with
    | (Some (n), least) when least.Length >= 2 ->
        least
        |> duplet
        |> List.map
            ((fun x -> (n, x))
             >> (fun (a, (b, c)) -> (a, b, c)))

        |> List.append (least |> triplet)
    | _ -> []

input
|> triplet
|> List.filter (fun (a, b, c) -> a + b + c = 2020)
|> List.map (fun (a, b, c) -> a * b * c)
|> List.iter (printfn "%d")
