#r "nuget: FsUnit.Xunit"
#load "./solution.fsx"

open FsUnit.Xunit
open Solution

let ``0,3,6`` = ``get nth number`` [ 0; 3; 6 ]

1 |> ``0,3,6`` |> should equal 0
2 |> ``0,3,6`` |> should equal 3
3 |> ``0,3,6`` |> should equal 6
4 |> ``0,3,6`` |> should equal 0
5 |> ``0,3,6`` |> should equal 3
6 |> ``0,3,6`` |> should equal 3
7 |> ``0,3,6`` |> should equal 1
8 |> ``0,3,6`` |> should equal 0
9 |> ``0,3,6`` |> should equal 3
10 |> ``0,3,6`` |> should equal 0

printfn "Success!"