module StartScreen

open System
open System.IO
open Xelmish.Viewables
open Xelmish.Model
open Constants

type Model = {
    playerNames: (string * string) option
}

let init () =
    { playerNames = None }

type Message = 
    | StartGame

let view model dispatch = 
    let text size = text "connection" size Colour.Black (-0.5, 0.)
    let textMid = resWidth / 2
    [
        yield text 50. "My (S)uper Cool Game!" (textMid, 40)

        yield onkeydown Keys.S (fun () -> dispatch StartGame)
    ]