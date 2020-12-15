open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

let u f a b = f b a
let split (s: string) (d: char) = s.Split(d)
let input = File.ReadAllLines "input.txt"
let rgMatch rg str = Regex.Match (rg, str)
let data =
    //{color} bags contain {number} {color} bags?(, {number} {color} bags?)*.
    let parseLine line =

        let split = u split
        let m = rgMatch @"(?<color>.*) bags? contain (?<content>.*)\." line

        let parseContent s =
            let m = rgMatch @"(?<count>\d+) (?<color>.*) bags?" s
            m.Groups.["color"].Value, m.Groups.["count"].Value |> Int32.Parse

        let color = m.Groups.["color"].Value

        let content =
            let v = m.Groups.["content"].Value
            if v = "no other bags" then
                []
            else
                v
                |> split ','
                |> Seq.map parseContent
                |> Seq.toList

        color, content

    let data = Dictionary()
    input
    |> Seq.map parseLine
    |> Seq.iter (fun (color, content) -> data.Add(color, content))
    data

let rec isBagContainShinyGoldBag (rule: KeyValuePair<string, (string * int) list>) =
    let content = rule.Value
    let innerBags = content |> List.map fst

    let hasGoldBag =
        innerBags
        |> List.exists (fun f -> f = "shiny gold")

    if hasGoldBag then
        true
    else
        let toPair color =
            if data.ContainsKey(color) then Some(KeyValuePair(color, data.[color])) else None
        innerBags
        |> Seq.map toPair
        |> Seq.filter (fun e -> e.IsSome)
        |> Seq.map (fun e -> e.Value)
        |> Seq.exists isBagContainShinyGoldBag

data
|> Seq.filter isBagContainShinyGoldBag
|> Seq.length
|> printfn "%i"
