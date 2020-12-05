open System.IO
open System
open System.Collections.Immutable
open System.Text.RegularExpressions

let getSeatIdFromTicket (str: string) =
    let binStr =        
        let rg1 = Regex("R|B")
        let rg0 = Regex(@"\D")
        let ones = rg1.Replace(str, "1")
        rg0.Replace(ones, "0")
    Convert.ToInt32(binStr, 2)

let puzzleInput = File.ReadAllLines "./input.txt"

let allTickets = puzzleInput |> Seq.map getSeatIdFromTicket |> Seq.toList

let min = allTickets |> List.min
let max = allTickets |> List.max
let fullSum = [min..max] |> List.sum
let actualSum = allTickets |> List.sum
let result = fullSum - actualSum

printfn "%i" result