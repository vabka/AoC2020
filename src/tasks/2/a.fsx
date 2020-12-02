open System
open System.Text.RegularExpressions
open System.IO

type IntRange = { Min: int; Max: int }

type PasswordPolicy = { Range: IntRange; Character: char }

let parseInt = Int32.Parse

let lineRegex =
    Regex(@"^(?<min>\d+)-(?<max>\d+) (?<char>\w): (?<password>.*)$")

let checkLine line =
    let r = lineRegex.Match(line)
    let min = parseInt (r.Groups.["min"].Value)
    let max = parseInt (r.Groups.["max"].Value)
    let character = Char.Parse(r.Groups.["char"].Value)
    let password = r.Groups.["password"].Value
    password :> seq<char>
    |> Seq.filter (fun x-> x = character)
    |> Seq.length
    |> fun l -> (l >= min && l <=max)

let result = File.ReadAllLines("input.txt") |> Seq.filter checkLine |> Seq.length
printfn "%d" result