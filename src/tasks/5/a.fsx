open System.IO
open System

type Range(Min: int, Max: int) =
    let length = Max - Min + 1
    member _.LowerHalf = Range(Min, Max - length / 2)

    member _.UpperHalf = Range(Min + length / 2, Max)

    member _.Exact =
        if Min - Max = 0 then Some(Max) else None

type Direction =
    | Front
    | Back
    | Left
    | Right

let getDirection l =
    match l with
    | 'F' -> Some(Front)
    | 'B' -> Some(Back)
    | 'L' -> Some(Left)
    | 'R' -> Some(Right)
    | _ -> None


let isSome (x: 'a option) = x.IsSome
let getVal (x: 'a option) = x.Value

let getSeatIdFromTicket str =
    let folder (rowRange: Range, columnRange: Range) (d: Direction) =
        match d with
        | Front -> (rowRange.LowerHalf, columnRange)
        | Back -> (rowRange.UpperHalf, columnRange)
        | Left -> (rowRange, columnRange.LowerHalf)
        | Right -> (rowRange, columnRange.UpperHalf)

    let (r, c) =
        str
        |> Seq.map getDirection
        |> Seq.filter isSome
        |> Seq.map getVal
        |> Seq.fold folder (Range(0, 127), Range(0, 7))

    let seatId row column = 8 * row + column

    match (r.Exact, c.Exact) with
    | Some (r), Some (c) -> seatId r c
    | _ -> raise <| InvalidOperationException()

let puzzleInput = File.ReadAllLines "./input.txt"

puzzleInput
|> Seq.map getSeatIdFromTicket
|> Seq.max
|> printfn "%i"
