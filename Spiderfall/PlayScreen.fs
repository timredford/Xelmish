module PlayScreen

open Xelmish.Model
open Xelmish.Viewables
open Elmish
open Constants

type Model = {
    todo: string option
}

let init () = { todo = None }


type Message = 
    | GameOver of string option


let update message model =
    match message with
    | GameOver _ -> model, Cmd.none // caught by parent ???

let view model dispatch =
    let centerText size = text primaryFontName size fontForegroundColor (-0.5, 0.)
    let windowCenter = windowWidth / 2
    [
        yield centerText messageFontSize "You're (P)laying!" (windowCenter, 40)
        yield onkeydown Keys.P (fun () -> dispatch (GameOver model.todo))
    ]