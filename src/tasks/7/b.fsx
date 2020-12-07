open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

let u f a b = f b a
let split (s: string) (d: char) = s.Split(d)
let input = File.ReadAllLines "input.txt"

let data =
    //{color} bags contain {number} {color} bags?(, {number} {color} bags?)*.
    let parseLine line =
        let rg =
            @"(?<color>.*) bags? contain (?<content>.*)\."

        let split = u split
        let rg = Regex(rg)
        let m = rg.Match(line)

        let parseContent s =
            let rg = @"(?<count>\d+) (?<color>.*) bags?"
            let rg = Regex(rg)
            let m = rg.Match(s)
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

let rec countBags color = 
   let content = data.[color]
   content |> Seq.map (fun (color, count) -> (countBags color) * count + count)|> Seq.sum

"shiny gold" |> countBags |> printfn "%i"