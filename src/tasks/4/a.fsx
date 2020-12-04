open System.Collections.Generic

let append (list: Dictionary<string, string> list) (line: string): Dictionary<string, string> list =
    let parse (line: string) =
        let parse (token: string) =
            let pair = token.Split(':')
            pair.[0], pair.[1]

        line.Split(' ') |> Seq.map parse |> Seq.toList

    match line with
    | "" -> Dictionary() :: list
    | _ ->
        match list with
        | last :: _ as l ->
            for (k, v) in (parse line) do
                last.Add(k, v)
            l
        | [] ->
            let d = Dictionary()
            for (k, v) in (parse line) do
                d.Add(k, v)
            [ d ]

let isValid (dict: Dictionary<string, string>): bool =
    let mandatory fieldName validation =
        dict.ContainsKey(fieldName)
        && validation (dict.[fieldName])

    let any _ = true

    mandatory "byr" any
    && mandatory "iyr" any
    && mandatory "eyr" any
    && mandatory "hgt" any
    && mandatory "hcl" any
    && mandatory "ecl" any
    && mandatory "pid" any


let lines = System.IO.File.ReadAllLines("input.txt")
let kvList: Dictionary<string, string> list = []
let batch = lines |> Seq.fold append kvList

batch
|> Seq.filter isValid
|> Seq.length
|> (printfn "%d")
