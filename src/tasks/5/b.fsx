open System.IO
open System

let getSeatIdFromTicket (ticket: string) =
    let binStr = (((ticket.Replace('R', '1')).Replace('B', '1')).Replace('L', '0')).Replace('F', '0')
    Convert.ToInt32(binStr, 2)

let puzzleInput = File.ReadAllLines "./input.txt"

let allTickets = puzzleInput |> Seq.map getSeatIdFromTicket |> Seq.toList

let min = allTickets |> List.min
let max = allTickets |> List.max
let fullSum = [min..max] |> List.sum
let actualSum = allTickets |> List.sum
let result = fullSum - actualSum

printfn "%i" result