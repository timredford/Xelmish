module GameOverScreen

open System
open System.IO
open Constants
open Xelmish.Viewables
open Xelmish.Model

type Model = {
    winner : string Option
}

let init winnerName =
    { winner = winnerName}

type Message = 
    | StartGame

let view model dispatch = 
    let text size = text "connection" size Colour.Black (-0.5, 0.)
    let textMid = resWidth / 2
    [
        yield text 80. "GAME OVER!" (textMid, 40)

        yield text 25. "(P)lay again?" (textMid, 100)
        yield text 25. "(Q)uit" (textMid, 150)

        yield onkeydown Keys.P (fun () -> dispatch StartGame)
        yield onkeydown Keys.Q exit
    ]