open System

let sequence (start: int list) =
    seq {
        yield! start
        let mutable ``previously spoken numbers`` = start
        while true do
            let ``last spoken number`` =
                ``previously spoken numbers`` |> List.last

            let ``indexes of last number`` =
                ``previously spoken numbers``
                |> List.mapi (fun i x -> i, x)
                |> List.filter (snd >> (=) ``last spoken number``)
                |> List.map (fst >> (+) 1)

            let next =
                match ``indexes of last number`` with
                | [] -> failwith "Impossible!"
                | [ _ ] -> 0
                | l ->
                    let l = l |> List.rev
                    let last = l.[0]
                    let prelast = l.[1]
                    last - prelast

            ``previously spoken numbers`` <- ``previously spoken numbers`` @ [ next ]

            yield next
    }

[ 13; 0; 10; 12; 1; 5; 8 ]
|> sequence
|> Seq.item 2019
|> printfn "%A"
