open System.Text.RegularExpressions
open System.Collections.Generic
open System

let matches rg text =
  let matches = Regex.Matches(rg, text)

  seq {
    for m in matches do
      let d = Dictionary()

      for group in m.Groups do
        d.Add(group.Name, group.Value)

      yield d :> IReadOnlyDictionary<string, string>
  }

let split (separator: string) (text: string) =
  text.Split([| separator |], StringSplitOptions.RemoveEmptyEntries)

let splitByLine =
  split (Environment.NewLine + Environment.NewLine)

let splitByEndl = split Environment.NewLine
let readFile = System.IO.File.ReadAllText
let readAllLines = readFile >> splitByEndl

let isSome (x: 'a option) = x.IsSome

let unwrap (x: 'a option) = x.Value

let parseInt = Int32.Parse

let firstSome (x: seq<'a option>)=  x |> Seq.filter isSome |> Seq.map unwrap |> Seq.head

let withNth n replace src = src |> List.mapi (fun i e -> if i = n then replace else e)