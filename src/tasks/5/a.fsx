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

File.ReadAllLines "./input.txt"
|> Seq.map getSeatIdFromTicket
|> Seq.max
|> printfn "%i"
