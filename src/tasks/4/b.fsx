open System.Collections.Generic
open System.Text.RegularExpressions

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

let parseInt str =
    let mutable x = 0
    if System.Int32.TryParse(str, &x) then Some(x) else None


let isValid (dict: Dictionary<string, string>): bool =
    let mandatory fieldName validation =
        dict.ContainsKey(fieldName)
        && validation (dict.[fieldName])

    let inRange min max num =
        match num with
        | Some (n) -> min <= n && n <= max
        | None -> false
    let numberInRange min max = parseInt >> (inRange min max)
    let regexp rg str =
        let rg = Regex(rg)
        rg.IsMatch str
        
    let (|Cm|_|) str =
        let rg = Regex(@"(\d+)cm")
        let m = rg.Match(str)
        if m.Success then parseInt m.Groups.[1].Value else None

    let (|In|_|) str =
        let rg = Regex(@"(\d+)in")
        let m = rg.Match(str)
        if m.Success then parseInt m.Groups.[1].Value else None

    mandatory "byr" (numberInRange 1920 2002)
    && mandatory "iyr" (numberInRange 2010 2020)
    && mandatory "eyr" (numberInRange 2020 2030)
    && mandatory "hgt" (fun str -> match str with
                                   | Cm h -> inRange 150 193 (Some h)
                                   | In h -> inRange 59 76 (Some h)
                                   | _ -> false)
    && mandatory "hcl" (regexp @"^#(\d|[a-f]){6}$")
    && mandatory "ecl" (regexp @"^amb|blu|brn|gry|grn|hzl|oth$")
    && mandatory "pid" (regexp @"^\d{9}$")


let lines = System.IO.File.ReadAllLines("input.txt")
let kvList: Dictionary<string, string> list = []
let batch = lines |> Seq.fold append kvList

batch
|> Seq.filter isValid
|> Seq.length
|> (printfn "%d")
