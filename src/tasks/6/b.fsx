open System.IO
open System.Collections.Immutable

let combineGroup (personsAnswers: string []) =
    let createAnswerSet (personAnswer: string) =
        ImmutableHashSet.Create<char>(personAnswer.ToCharArray())
    personsAnswers
    |> Seq.map createAnswerSet
    |> Seq.reduce (fun c n -> c.Intersect(n))

File.ReadAllText("input.txt").Split("\n\n")
|> Seq.map (fun s -> s.Split("\n") |> combineGroup)
|> Seq.map (fun s -> s.Count)
|> Seq.sum
|> printfn "%i"
