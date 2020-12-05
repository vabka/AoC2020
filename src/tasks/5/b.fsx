open System.IO
open System

let getSeatIdFromTicket (ticket: string) =
    let replace (n: char) (o: char) (str: string) = str.Replace(o, n)
    let toZero = replace '1'
    let toOne = replace '0'

    let bits =
        ticket
        |> toOne 'R'
        |> toZero 'L'
        |> toOne 'B'
        |> toZero 'F'

    Convert.ToInt32(bits, 2)

let puzzleInput = File.ReadAllLines "./input.txt"

let allTickets =
    puzzleInput
    |> Seq.map getSeatIdFromTicket
    |> Seq.toList

let min = allTickets |> List.min
let max = allTickets |> List.max

let fullSum = [ min .. max ] |> List.sum
let actualSum = allTickets |> List.sum

let ourSeat = fullSum - actualSum

printfn "%i" ourSeat
