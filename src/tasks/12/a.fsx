#load "../Common.fsx"
open Common
open System

type Compas =
    | North
    | South
    | East
    | West

type MovementDirection =
    | Forward
    | Absolute of Compas

type RotationDirection =
    | Left
    | Right


type Command =
    | Movement of MovementDirection * int
    | Rotation of RotationDirection * int

type ShipState =
    { North: int
      East: int
      Direction: Compas }



let parseCommand (line: string) =
    match (line.[0], line.[1..] |> parseInt) with
    | ('N', value) -> Movement(Absolute(North), value)
    | ('S', value) -> Movement(Absolute(South), value)
    | ('E', value) -> Movement(Absolute(East), value)
    | ('W', value) -> Movement(Absolute(West), value)
    | ('L', value) -> Rotation(Left, value)
    | ('R', value) -> Rotation(Right, value)
    | ('F', value) -> Movement(Forward, value)
    | _ -> failwith "Unreachable"

let neNorthtPosition (shipPosition: ShipState) (command: Command) =
    let rec calculateNewDirection (currentDirection: Compas)
                                  (rotationDirection: RotationDirection)
                                  (rotationDegrees: int)
                                  =
        if rotationDegrees = 270 then
            calculateNewDirection currentDirection (if rotationDirection = Left then Right else Left) 90
        else
            match rotationDegrees with
            | 90 ->
                match (currentDirection, rotationDirection) with
                | North, Left -> West
                | North, Right -> East
                | West, Left -> South
                | West, Right -> North
                | South, Left -> East
                | South, Right -> West
                | East, Left -> North
                | East, Right -> South
            | 180 ->
                match currentDirection with
                | North -> South
                | East -> West
                | West -> East
                | South -> North
            | _ -> failwith "Invalid"

    let rec calculateNewPosition (shipPosition: ShipState) (movementDirection: MovementDirection) value =
        match movementDirection with
        | Forward -> calculateNewPosition shipPosition (Absolute shipPosition.Direction) value
        | Absolute (direction) ->
            match direction with
            | North ->
                { shipPosition with
                      North = shipPosition.North + value }
            | South ->
                { shipPosition with
                      North = shipPosition.North - value }
            | East ->
                { shipPosition with
                      East = shipPosition.East + value }
            | West ->
                { shipPosition with
                      East = shipPosition.East - value }

    match command with
    | Rotation (d, v) ->
        { shipPosition with
              Direction = (calculateNewDirection shipPosition.Direction d v) }
    | Movement (d, v) -> calculateNewPosition shipPosition d v

let shipStartingPosition =
    { North = 0
      East = 0
      Direction = East }

let shipFinalPosition =
    readAllLines "input.txt"
    |> Seq.map parseCommand
    |> Seq.fold neNorthtPosition shipStartingPosition

(Math.Abs(shipFinalPosition.North)
 + Math.Abs(shipFinalPosition.East))
|> printfn "%d"
