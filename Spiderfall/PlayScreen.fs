﻿module PlayScreen

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

type Cell = { Col: Column; Row: Row }

let plusOne item = item

let getCoords cell = 
    let colIdx = Column.List |> List.findIndex (fun c -> c = cell.Col) |> plusOne
    let rowIdx = Row.List |> List.findIndex (fun r -> r = cell.Row) |> plusOne
    (colIdx, rowIdx)

type Board = Map<Cell, Piece option>

type GameResult = Player option

type Turn = 
    | Player of Player 
    | GameOverT of GameResult

type Model = {
    board : Board
    currentTurn : Turn
}

let createRow row pieces =
    let cells = Column.List |> List.map (fun col -> { Col = col; Row = row })
    List.zip cells pieces 

let createBoard:Board =
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

let getCellToDropTo ((c1,p1),(c2,p2)) =
    match p1,p2 with
    | None,Some _ -> Some c1
    | Some _ ,None -> None
    | Some _ ,Some _ -> None
    | None, None -> 
        match c2 with
        | {Row=One }-> Some c2
        | _ -> None

let dropPiece (col, piece) model =
    let inColumnList = 
        model.board 
        |> Map.toList 
        |> List.where (fun (c,d) -> c.Col = col) 
        |> List.sortByDescending (fun (c,p) -> getCoords (c) |> snd)

    let firstOccupiedCell =
        inColumnList 
        |> List.pairwise 
        |> List.tryPick (getCellToDropTo) // TODO figure out what to do here

    match firstOccupiedCell with
    | Some c -> { model with 
                    board=model.board.Add(c, Some piece); 
                    currentTurn=(nextTurn model) }
    | None -> model

type Message = 
    | DropPiece of Player * Rank * Column
    | GameOver of GameResult


let getListOfCellOptions numCells nextCell startingCell =
    let findNextCellFromCell = nextCell (startingCell)
    let lastNum = numCells - 1
    [0..lastNum] |> List.map findNextCellFromCell

let connect4 = getListOfCellOptions 4


let getNext xModifier yModifier cell i = 
    let (xCol,yRow) = getCoords cell
    let col = Column.List |> List.tryItem (xModifier xCol i)
    let row = Row.List |> List.tryItem (yModifier yRow i)
    let res = match row, col with
                | Some r, Some c -> Some {cell with Row=r; Col=c}
                | _ -> None
    res

let getNextVertical = getNext (fun x i -> x) (fun y i -> y + i)
let getNextHorizontal = getNext (fun x i -> x + i) (fun y i -> y )
let getNextPosDiagonal = getNext (fun x i -> x + i) (fun y i -> y + i )
let getNextNegDiagonal = getNext (fun x i -> x + i) (fun y i -> y - i )

let getWinner (pieceList: Piece option list) =
    let distictList = pieceList |> List.distinct 
    match distictList with
    | [po] -> match po with
            | Some (player,_) -> Some player
            | None -> None
    | _ -> None

let tryFind (board:Board) (cell:Cell option) =
    match cell with
    | Some c -> board.TryFind c |> Option.flatten
    | None -> None

let checkCellForStartOfConnect (board:Board) (cell:Cell) =
    let tryFindInBoard = board |> tryFind
    let directions = [getNextVertical; getNextHorizontal; getNextPosDiagonal; getNextNegDiagonal]
    let allCellsForEachDirection = directions |> List.map (fun dir -> connect4 dir cell)
    let allPiecesPerDirection = allCellsForEachDirection |> List.map (fun cells -> cells |> List.map tryFindInBoard )
    let allWinnersPerDirection = allPiecesPerDirection |> List.map getWinner
    let allWinners = allWinnersPerDirection |> List.where (fun x -> x.IsSome)
    let winner = match allWinners with
                    | [] -> None
                    | [a] -> a
                    | _ -> None
    winner


let tryCheckWinner (board:Board) :GameResult option =
    let boardIsFull = board |> Map.forall (fun _ p -> p.IsSome)
    let checkCellInBoard = checkCellForStartOfConnect board
    let cellStates = board 
                    |> Map.toList 
                    |> List.map (fun (cell, _) -> cell) 
                    |> List.map checkCellInBoard
                    |> List.distinct 
                    |> List.where (fun playerOpt -> playerOpt.IsSome)
    let winner = match cellStates with
                    | [a] -> Some (a)
                    | [a;b] -> Some (None)
                    | _ -> match boardIsFull with 
                            | true -> Some (None)
                            | false -> None
    winner


let update message model =
    match message with
    | DropPiece (player, rank, col)-> 
        let piece  = (player, rank)
        let m = dropPiece (col,piece) model
        let gameResult = tryCheckWinner m.board
        let cmd = match gameResult with
                    | Some result -> Cmd.ofMsg (GameOver result)
                    | None -> Cmd.none
        m, cmd
    | GameOver _ -> model, Cmd.none // caught by parent ???








let toColor piece =
    match piece with
    | Some (Player1, Spider) -> Colour.Red
    | Some (Player2, Spider) -> Colour.DarkSlateGray
    | None -> Colour.Tan

let makeRect (cell , piece) = 
    let (c,r) = getCoords cell
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
        | Player player -> 
            yield centerText messageFontSize (getPlayerName player) (windowCenter, 60)
            yield onkeydown Keys.A (fun () -> dispatch (DropPiece (player, Spider, Column.A)))
            yield onkeydown Keys.B (fun () -> dispatch (DropPiece (player, Spider, Column.B)))
            yield onkeydown Keys.C (fun () -> dispatch (DropPiece (player, Spider, Column.C)))
            yield onkeydown Keys.D (fun () -> dispatch (DropPiece (player, Spider, Column.D)))
            yield onkeydown Keys.E (fun () -> dispatch (DropPiece (player, Spider, Column.E)))
            yield onkeydown Keys.F (fun () -> dispatch (DropPiece (player, Spider, Column.F)))
            yield onkeydown Keys.G (fun () -> dispatch (DropPiece (player, Spider, Column.G)))
        | GameOverT _ -> yield centerText messageFontSize "GAME OVER" (windowCenter, 60)
        
        yield onkeydown Keys.Q (fun () -> dispatch (GameOver None))
        yield onkeydown Keys.NumPad1 (fun () -> dispatch (GameOver (Some Player1)))
        yield onkeydown Keys.NumPad2 (fun () -> dispatch (GameOver (Some Player2)))
    ] |> List.append (drawBoard (model.board))