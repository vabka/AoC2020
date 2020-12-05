open System.IO
open System
let getSeatIdFromTicket (ticket: string) =
    let binStr = (((ticket.Replace('R', '1')).Replace('B', '1')).Replace('L', '0')).Replace('F', '0')
    Convert.ToInt32(binStr, 2)
File.ReadAllLines "./input.txt"
|> Seq.map getSeatIdFromTicket
|> Seq.max
|> printfn "%i"