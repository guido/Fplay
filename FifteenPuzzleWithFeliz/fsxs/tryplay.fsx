//#load "../src/FifteenPuzzle.fs"
open System

type Position = { X: int; Y: int }

let random = Random()

let north { X=x; Y=y } =
    if y = 1
    then None
    else Some { X=x; Y=y-1 }

let south { X=x; Y=y } =
    if y=4 
    then None
    else Some {X=x; Y=y+1}

let west {X=x; Y=y} =
    if x = 1
    then None
    else Some { X=x-1; Y=y }

let east { X=x; Y=y } =
    if x = 4
    then None
    else Some {X=x+1; Y=y}

let numberToPosition n = { X= 1+(n-1)%4; Y=1+(n-1)/4 }

let positionToNumber {X=x; Y=y} = 4*(y-1) + x

let numberToNeighbour n position= 
    match n%4 with
    | 0 -> position |> north
    | 1 -> position |> south
    | 2 -> position |> west
    | _ -> position |> east


let positions = [|1 .. 16|]

let freeindex (array:int array) = Array.IndexOf (array, 16)
let freenumber a = 1 + (a |>freeindex)
let r = 2
let fp = freenumber positions |> numberToPosition

let swapNumberOption = numberToNeighbour 0 fp

