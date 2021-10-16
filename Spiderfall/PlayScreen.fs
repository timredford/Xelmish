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
    let ignore = model.todo
    let text size = text "connection" size Colour.Black (-0.5, 0.)
    let textMid = resWidth / 2
    [
        yield text 25. "You're (P)laying!" (textMid, 40)
        yield onkeydown Keys.P (fun () -> dispatch (GameOver model.todo))
    ]