open System.IO
open System.Collections.Immutable

File.ReadAllText("input.txt").Split("\n\n")
|> Seq.map (fun f -> f.Replace("\n", ""))
|> Seq.map (fun f -> f.ToCharArray())
|> Seq.map ImmutableHashSet.Create<char>
|> Seq.map (fun f -> f.Count)
|> Seq.sum
|> printfn "%i"
