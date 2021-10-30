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
    let winnerMessage = match model.winner with 
                        | Some s -> sprintf "%A Won!" s 
                        | None -> "It's a tie!"
    let text size = text primaryFontName size fontForegroundColor (-0.5, 0.)
    
    let textMid = windowWidth / 2
    [
        yield text headerFontSize "GAME OVER!" (windowCenter, 40)
        yield text headerFontSize winnerMessage (windowCenter, 90)

        yield! grayButton "Play Again" (fun () -> dispatch Reset) (100, 50) (windowCenter - 50, 140) 20. 
        yield! grayButton "Quit" exit (100, 50) (windowCenter - 50, 200) 20. 
        //yield onclick exit (100, 50) (textMid - 50, 140)

        yield onkeydown Keys.P (fun () -> dispatch Reset)
        yield onkeydown Keys.Q exit
        yield onkeydown Keys.Escape exit
    ]