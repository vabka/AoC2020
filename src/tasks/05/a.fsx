open System.IO
open System

let getSeatIdFromTicket (ticket: string) =
    let replace (o: char) (n: char) (str: string) = str.Replace(o, n)
    let bits =
        ticket
        |> replace 'R' '1'
        |> replace 'L' '0'
        |> replace 'B' '1'
        |> replace 'F' '0'

    Convert.ToInt32(bits, 2)

File.ReadAllLines "./input.txt"
|> Seq.map getSeatIdFromTicket
|> Seq.max
|> printfn "%i"
