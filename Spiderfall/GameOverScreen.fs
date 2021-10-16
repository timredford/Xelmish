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
    | Reset

let view model dispatch = 
    let text size = text primaryFontName size fontForegroundColor (-0.5, 0.)
    let textMid = windowWidth / 2
    [
        yield text headerFontSize "GAME OVER!" (textMid, 40)

        yield text messageFontSize "(P)lay again?" (textMid, 100)
        yield text messageFontSize "(Q)uit" (textMid, 125)

        yield onkeydown Keys.P (fun () -> dispatch Reset)
        yield onkeydown Keys.Q exit
    ]