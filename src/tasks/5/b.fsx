open System.IO
open System
open System.Collections.Immutable

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


let seatId row column = 8 * row + column
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
    match (r.Exact, c.Exact) with
    | Some (r), Some (c) -> seatId r c
    | _ -> raise <| InvalidOperationException()

let puzzleInput = File.ReadAllLines "./input.txt"

let allSeats =
    let seatId (r,c) = seatId r c
    let allRows = [0..127]
    let allColumns = [0..7]
    let allSeatsInRow row = allColumns |> List.map (fun column -> row, column)
    allRows
    |> List.fold (fun l r -> l@(allSeatsInRow r)) []
    |> List.toArray

let myList = puzzleInput |> Seq.map getSeatIdFromTicket |> Seq.toArray |> ImmutableHashSet.Create<int>

// Я замучался. Загоняем всё в эксель...
let allRows = [0..127]
let allColumns = [0..7]
let printRow (r:int) =
    allColumns |> List.map (seatId r) |> List.iter (fun id -> printf "%i\t" (if myList.Contains(id) then 1 else 0))
    printfn ""
allRows |> List.iter printRow