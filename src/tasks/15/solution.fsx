open System.Collections.Generic
open System

let ``get nth number`` init n=
    seq {
        yield! init
        let cache = Dictionary()

        for (i, n) in init |> List.mapi (fun i n -> struct (i, n)) do
            cache.[n] <- [ i + 1 ]

        let mutable ``last turn`` = init.Length
        let mutable ``last spoken number`` = init |> List.last

        while true do
            let ``current turn`` = ``last turn`` + 1

            let ``current number`` =
                match cache.[``last spoken number``] with
                | [ _ ] -> 0
                | [ prev; prePrev ] -> prev - prePrev
                | _ -> failwith "Impossible!"

            yield ``current number``

            ``last spoken number`` <- ``current number``
            // пересчитываем кеш
            // приходится делать проверки на наличие ключа и match [], чтобы компилятор не ругался и не словить рантайм ексепшн
            if cache.ContainsKey ``last spoken number`` then
                match cache.[``last spoken number``] with
                | [] -> failwith "Impossible!"
                | prev :: _ -> cache.[``last spoken number``] <- [ ``current turn``; prev ]
            else
                cache.[``last spoken number``] <- [ ``current turn`` ]

            ``last turn`` <- ``current turn``
    }
    |> Seq.item (n - 1)

let ``parse list`` (str: string) = str.Split(',') |> Seq.map Int32.Parse |> Seq.toList
