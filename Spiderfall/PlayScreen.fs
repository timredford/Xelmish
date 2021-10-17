module PlayScreen

open Xelmish.Model
open Xelmish.Viewables
open Elmish
open Constants

type Player = | Player1 | Player2

type Rank = | Spider

type Piece = Player * Rank

type Column = | A | B | C | D | E | F | G  
    with static member List = [A;B;C;D;E;F;G;]
type Row = | One | Two | Three | Four | Five | Six | Seven 
    with static member List = [One; Two; Three; Four; Five; Six; Seven] 

let rowToInt row =
    match row with
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7

let colToInt col =
    match col with 
    | A -> 1
    | B -> 2
    | C -> 3
    | D -> 4
    | E -> 5
    | F -> 6
    | G -> 7


type Cell = { Col: Column; Row: Row }
type Board = Map<Cell, Piece option>

type Turn = 
    | Player of Player 
    | GameOverT of Player option

type Model = {
    board : Board
    currentTurn : Turn
}

let createRow row pieces =
    let cells = Column.List |> List.map (fun col -> { Col = col; Row = row })
    List.zip cells pieces 

let createBoard =
    Row.List 
    |> List.map (fun row -> createRow row (Column.List |> List.map (fun col -> None))) 
    |> Seq.concat
    |> Map

let createAllPlayer1GameBoard =
    Map (   (createRow Seven    [Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);]) @
            (createRow Six      [Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);]) @
            (createRow Five     [Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);]) @
            (createRow Four     [Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);]) @
            (createRow Three    [Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);]) @
            (createRow Two      [Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);]) @
            (createRow One      [Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);    Some (Player1, Spider);      Some (Player1, Spider);]) )

let createCheckeredGameBoard =
    Map (   (createRow Seven    [Some (Player2, Spider);    Some (Player1, Spider);      Some (Player2, Spider);    Some (Player1, Spider);      Some (Player2, Spider);    Some (Player1, Spider);      Some (Player2, Spider);]) @
            (createRow Six      [Some (Player1, Spider);    Some (Player2, Spider);      Some (Player1, Spider);    Some (Player2, Spider);      Some (Player1, Spider);    Some (Player2, Spider);      Some (Player1, Spider);]) @
            (createRow Five     [Some (Player2, Spider);    None;                        Some (Player2, Spider);    Some (Player1, Spider);      Some (Player2, Spider);    Some (Player1, Spider);      Some (Player2, Spider);]) @
            (createRow Four     [Some (Player1, Spider);    Some (Player2, Spider);      Some (Player1, Spider);    Some (Player2, Spider);      Some (Player1, Spider);    Some (Player2, Spider);      Some (Player1, Spider);]) @
            (createRow Three    [Some (Player2, Spider);    Some (Player1, Spider);      Some (Player2, Spider);    Some (Player1, Spider);      Some (Player2, Spider);    Some (Player1, Spider);      Some (Player2, Spider);]) @
            (createRow Two      [Some (Player1, Spider);    Some (Player2, Spider);      Some (Player1, Spider);    Some (Player2, Spider);      Some (Player1, Spider);    Some (Player2, Spider);      Some (Player1, Spider);]) @
            (createRow One      [Some (Player2, Spider);    Some (Player1, Spider);      Some (Player2, Spider);    Some (Player1, Spider);      Some (Player2, Spider);    Some (Player1, Spider);      Some (Player2, Spider);]) )
    
let nextTurn model =
    match model.currentTurn with
    | Player p -> 
        match p with
        | Player1 -> Player Player2
        | Player2 -> Player Player1
    | GameOverT _ -> model.currentTurn // no change
    
let getPlayerName p = 
    match p with
    | Player1 -> "Player 1"
    | Player2 -> "Player 2"

let init () = { 
    board = createBoard
    currentTurn = Player Player1
}


type Message = 
    | DropPiece of Rank * Column
    | GameOver of Player option


let update message model =
    match message with
    | DropPiece _-> 
        {model with currentTurn=(nextTurn model)}, Cmd.none
    | GameOver _ -> model, Cmd.none // caught by parent ???

let toColor piece =
    match piece with
    | Some (Player1, Spider) -> Colour.Red
    | Some (Player2, Spider) -> Colour.DarkSlateGray
    | None -> Colour.Tan

let makeRect (cell , piece) = 
    let (c,r) = (colToInt cell.Col, rowToInt cell.Row)
    let width = 20
    let height = 20
    let y = ( Row.List.Length - r ) * height
    let x = c * width
    let pos = (x,y)
    let s = (width, height) 
    let color = toColor piece
    colour color s pos

let drawBoard board =
    board |> Map.toList |> List.map makeRect

let view model dispatch =
    let centerText size = text primaryFontName size fontForegroundColor (-0.5, 0.)
    
    let windowCenter = windowWidth / 2
    [
        yield centerText messageFontSize "You're (P)laying!" (windowCenter, 40)
        match model.currentTurn with
        | Player player -> yield centerText messageFontSize (getPlayerName player) (windowCenter, 60)
        | GameOverT _ -> yield centerText messageFontSize "GAME OVER" (windowCenter, 60)
        
        yield onkeydown Keys.Q (fun () -> dispatch (GameOver None))
        yield onkeydown Keys.A (fun () -> dispatch (DropPiece (Spider, Column.A)))
        yield onkeydown Keys.B (fun () -> dispatch (DropPiece (Spider, Column.B)))
        yield onkeydown Keys.C (fun () -> dispatch (DropPiece (Spider, Column.C)))
        yield onkeydown Keys.D (fun () -> dispatch (DropPiece (Spider, Column.D)))
        yield onkeydown Keys.E (fun () -> dispatch (DropPiece (Spider, Column.E)))
        yield onkeydown Keys.F (fun () -> dispatch (DropPiece (Spider, Column.F)))
        yield onkeydown Keys.G (fun () -> dispatch (DropPiece (Spider, Column.G)))
    ] |> List.append (drawBoard (model.board))