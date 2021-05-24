namespace App

open Feliz
open Feliz.Router

type Components =
    /// <summary>
    /// The simplest possible React component.
    /// Shows a header with the text Hello World
    /// </summary>
    [<ReactComponent>]
    static member HelloWorld() =
        Html.div [ Html.h1 [ prop.text "Hello World"
                             prop.style [ style.color.green ] ]
                   Html.p "hopefully.." ]

    /// <summary>
    /// A stateful React component that maintains a counter
    /// </summary>
    [<ReactComponent>]
    static member Counter() =
        let (count, setCount) = React.useState (0)

        Html.div [ Html.h1 [ prop.text count
                             prop.style [ style.color.goldenRod ] ]
                   Html.button [ prop.onClick (fun _ -> setCount (count + 1))
                                 prop.text "Increment" ] ]

    static member MoreElements() =
        Html.div [ 
            Components.HelloWorld()
            Components.HelloWorld() 
        ]

    /// <summary>
    /// A React component that uses Feliz.Router
    /// to determine what to show based on the current URL
    /// </summary>
    [<ReactComponent>]
    static member Router() =
        let (currentUrl, updateUrl) = React.useState (Router.currentUrl ())

        React.router [ 
           router.onUrlChanged updateUrl
           router.children [ match currentUrl with
             | [] -> Html.h1 "Index"
             | [ "hello" ] -> Components.HelloWorld()
             | [ "counter" ] -> Components.Counter()
             | otherwise -> Html.h1 "Not found" ] ]


    [<ReactComponent>]
    static member FifteenPuzzle() =
        let (gameStarted, setGameStarted) = React.useState (false)

        let (appState, setAppState) =
            React.useStateWithUpdater (FifteenPuzzle.initialState ())
        
        let stylesheet = FifteenPuzzle.stylesheet

        Html.div [ 
           prop.style [ style.textAlign.center ]
           prop.children [ 
               Html.h1 "Fifteen Puzzle"
               if not gameStarted then
                   Html.button [ 
                       prop.text "Satrt game"
                       prop.onClick (fun _ -> setGameStarted (true)) ]
               else
                   Html.div [ 
                      prop.className stylesheet.["slot-container"]
                      prop.children [ 
                          for (position, tag) in appState.Slots do
                          Html.div [ prop.text (
                                         if position = appState.FreePos then
                                             ""
                                         else
                                             tag
                                     )
                                     prop.onClick (fun _ ->
                                         setAppState (fun prevState ->
                                             if FifteenPuzzle.canMove position prevState 
                                             then
                                                 FifteenPuzzle.slotSelected
                                                     prevState
                                                     position
                                                     tag
                                             else prevState
                                             )
                                         )
                                     prop.className [
                                         if position = appState.FreePos 
                                         then stylesheet.["free-slot"]
                                         elif FifteenPuzzle.inFinalPosition position tag
                                         then stylesheet.["final-slot"]
                                         else stylesheet.["slot"]
                                         if FifteenPuzzle.canMove position appState 
                                         then stylesheet.["movable-slot"]
                                         else null
                                     ] ] 
                          if FifteenPuzzle.gameSolved appState 
                          then Html.p "You win!!"                          
                       ] 
                        
                ] 

           ] 

       ]


       
    
