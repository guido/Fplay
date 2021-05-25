module FifteenPuzzle

open System

type Position = { X: int; Y: int }

type Slot = Position * string

type AppState = { Slots: Slot list; FreePos: Position }

type Msg =
    | StartNewGame
    | SelectSlot of Slot

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


let generateIntialPosition =
    let positions = [|1 .. 16|]
    let swap n m = 
        let t = positions.[n-1]
        positions.[n-1] <- positions.[m-1]
        positions.[m-1] <- t
        
        
    let random = Random()
    
    let freeindex (array:int array) = Array.IndexOf (array, 16)
    let freenumber a = 1 + (a |>freeindex)

    let iterations = 20

    for j in [1..iterations] do
      let r = random.Next()
      let fp = freenumber positions |> numberToPosition
      let swapNumberOption = numberToNeighbour r fp
      
      
      swapNumberOption 
      |> Option.map positionToNumber 
      |> Option.map (fun n -> swap n (freenumber positions))
      |> ignore
    
    positions

let initialState () : AppState =
    let randomTags =
        generateIntialPosition|> Array.toList
        //List.sortBy id [1 .. 16]
        //List.sortBy (fun _ -> random.Next()) [ 1 .. 16 ]
    //generate slot positions
    [ for x in 0 .. 3 do
          for y in 0 .. 3 do
              yield { X = x; Y = y } ]
    |> List.mapi (fun i pos -> pos, string (List.item i randomTags))
    |> fun slots ->
        //find the free slot : 16
        let (pos, _) =
            Seq.find (fun (_, tag) -> tag = "16") slots
        //return state
        { Slots = slots; FreePos = pos }

let slotSelected state position tag =
    if position = state.FreePos then
        let newstate = state
        newstate
    else
        { state with
              FreePos = position
              Slots =
                  state.Slots
                  |> List.map
                      (fun (slotPosition, slotTag) ->
                          if slotPosition = position then
                              (slotPosition, "16")
                          elif slotPosition = state.FreePos then
                              (slotPosition, tag)
                          else
                              (slotPosition, slotTag)) }

//let stylesheet = Stylesheet.load "./fifteen-puzzle.module.css"

(* let l1Distance position position' = 
    Math.Abs(position.X - position'.X) + Math.Abs(position.Y - position'.Y) *)

let l1D {X=x1; Y=y1} {X=x2; Y=y2} =
    Math.Abs(x1-x2) + Math.Abs(y1-y2)


let canMove position state = 
    l1D position state.FreePos = 1
//Browser.Dom.console.log stylesheet

let inFinalPosition {X=x; Y=y} (tag:string) = 
    x*4 + y + 1 = int tag

let gameSolved state =
    state.Slots 
    |> List.filter (fun (position, _) -> position <> state.FreePos)
    |> List.forall (fun (position, tag) -> inFinalPosition position tag)

(* 
let neightBours {X=x; Y=y} =
    match x, y with
    | x =1, y=1 -> [ { X=2; Y=1}; [X=1; Y=2]]
    | x=4; y=1 -> [ {X=3; Y=1}; {X=4; Y=2}]
    | _; y=1 -> [ { X=x+1; Y=y}; [X=x; Y=y+1]; [X=x-1; Y=y]] 
     *)








