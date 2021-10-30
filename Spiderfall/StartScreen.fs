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

let instructions = "Welcome to Spiderfall. \nThis is a local 2 player game.\nSpiderfall is Connect4 but with spiders.\nTake turns dropping spiders into the columns by clicking on the buttons above the board.\nFirst player to connect 4 in a row wins.\nConnections can be made horizontally, vertically or diagonally."

let view model dispatch = 
    let centerText size = text primaryFontName size fontForegroundColor (-0.5, 0.)
    let windowCenter = windowWidth / 2
    [
        yield centerText headerFontSize "Spiderfall" (windowCenter, 40)
        yield! grayButton "Start" (fun () -> dispatch StartGame) (100, 50) (windowCenter - 50, 140) 20. 

        yield centerText 80. instructions (windowCenter, 230)

        yield onkeydown Keys.S (fun () -> dispatch StartGame)
    ]