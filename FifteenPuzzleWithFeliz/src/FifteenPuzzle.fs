module FifteenPuzzle

open System

type Position = { X: int; Y: int }

type Slot = Position * string

type AppState = { Slots: Slot list; FreePos: Position }

type Msg =
    | StartNewGame
    | SelectSlot of Slot

let random = Random()

let initialState () : AppState =
    let randomTags =
        List.sortBy (fun _ -> random.Next()) [ 1 .. 16 ]
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

let stylesheet = Stylesheet.load "./fifteen-puzzle.module.css"

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
