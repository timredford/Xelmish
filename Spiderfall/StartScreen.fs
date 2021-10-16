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
    let centerText size = text primaryFontName size fontForegroundColor (-0.5, 0.)
    let windowCenter = windowWidth / 2
    [
        yield centerText headerFontSize "My (S)uper Cool Game!" (windowCenter, 40)

        yield onkeydown Keys.S (fun () -> dispatch StartGame)
    ]