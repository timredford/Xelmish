open Elmish
open Xelmish.Model
open Xelmish.Viewables
open Constants


type Model =
    | Start of StartScreen.Model
    | Playing of PlayScreen.Model
    | GameOver of GameOverScreen.Model


let init () =
    Start (StartScreen.init ()), Cmd.none

type Message = 
    | StartScreenMessage of StartScreen.Message
    | PlayScreenMessage of PlayScreen.Message
    | GameOverScreenMessage of GameOverScreen.Message

let update message model =
    match model, message with
    | Start _, StartScreenMessage msg ->
        match msg with
        | StartScreen.StartGame -> Playing (PlayScreen.init ()), Cmd.none

    | Playing playScreen, PlayScreenMessage msg -> 
        match msg with
        | PlayScreen.GameOver score -> GameOver (GameOverScreen.init score), Cmd.none
        | _ -> 
            let newModel, newCommand = PlayScreen.update msg playScreen
            Playing newModel, Cmd.map PlayScreenMessage newCommand

    | GameOver _, GameOverScreenMessage msg ->
        match msg with
        | GameOverScreen.StartGame -> Playing (PlayScreen.init ()), Cmd.none

    | _ -> model, Cmd.none // invalid combination

// all of the above is straight from the Elmish.WPF sample (with only some indentation changes)
// the view function below, replacing Elmish.WPF's bindings function, is Xelmish specific,
// though note it still follows the dispatch model common to Elmish implementations.

let view model dispatch =

    match model with
    | Start startScreen ->
        StartScreen.view startScreen (StartScreenMessage >> dispatch)
    | Playing playScreen ->
        PlayScreen.view playScreen (PlayScreenMessage >> dispatch)
    | GameOver gameOverScreen ->
        GameOverScreen.view gameOverScreen (GameOverScreenMessage >> dispatch)

[<EntryPoint>]
let main _ =
    let config = {
        resolution = Windowed (resWidth, 300)
        clearColour = Some Colour.WhiteSmoke // if set to None, then each draw will layer over the previous. which looks weird.
        mouseVisible = true
        assetsToLoad = [
            PipelineFont ("connection", "./content/Connection")   
        ]
    }

    Program.mkProgram init update view // standard, out of the box Elmish initialisation
    |> Program.withConsoleTrace // standard, out of the box Elmish console tracing.
    |> Xelmish.Program.runGameLoop config // Xelmish specific run function
    0
