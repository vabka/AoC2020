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

type Ship =
    { North: int
      East: int
      Direction: Compas }

type Waypoint = { North: int; East: int }


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

let folder (ship: Ship, waypoint:Waypoint) (command: Command) =
    let rec rotate waypoint direction degrees = 
        match (direction, degrees) with
        | (_, 180) -> {North = waypoint.North * -1; East = waypoint.East * -1}
        | (Right, 90) -> {North = waypoint.East * -1; East = waypoint.North}
        | (Left, 90) -> {North = waypoint.East; East = waypoint.North * -1}
        | (d, 270) -> rotate waypoint (if d = Left then Right else Left) 90
        | _ -> failwith "Invalid!"
    let moveForward (ship:Ship) waypoint value =
        {ship with North = (ship.North + waypoint.North * value); East = (ship.East + waypoint.East * value)}
    let move (waypoint:Waypoint) direction value =
        match direction with
            | North ->
                { waypoint with
                      North = waypoint.North + value }
            | South ->
                { waypoint with
                      North = waypoint.North - value }
            | East ->
                { waypoint with
                      East = waypoint.East + value }
            | West ->
                { waypoint with
                      East = waypoint.East - value }
    match command with 
    | Movement (Forward, value) -> moveForward ship waypoint value, waypoint
    | Movement (Absolute(direction), value) -> ship, move waypoint direction value
    | Rotation (direction, degrees) -> ship, rotate waypoint direction degrees

let shipStartingPosition =
    { North = 0
      East = 0
      Direction = East }

let waypointStartingPosition = { North = 1; East = 10 }

let (ship, _) =
    readAllLines "input.txt"
    |> Seq.map parseCommand
    |> Seq.fold folder (shipStartingPosition, waypointStartingPosition)

Math.Abs(ship.North) + Math.Abs(ship.East) |> printfn "%d"
