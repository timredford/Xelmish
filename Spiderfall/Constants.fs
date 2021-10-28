module Constants
open Xelmish.Model

let fontForegroundColor = Colour.Black
let backgroundColor = Colour.WhiteSmoke

let primaryFontName = "connection"
let primaryFontLocation = "./content/Connection"

let headerFontSize = 50.
let messageFontSize = 25.
let labelFontSize = 10.
let buttonFontSize = 20.

let windowWidth = 700
let windowHeight = 400

let windowCenter = windowWidth / 2
let currentPlayerSpriteToken_x = windowCenter + 100

let cell_width = 30
let cell_height = 30
let board_width = cell_width * 7

let boardOffsetX = (windowWidth - board_width) / 2
let boardOffsetY = 80

let sprite_width = cell_width
let sprite_heigth = cell_height

let buttonRowHeight = 30
let buttonRowPosition = boardOffsetY



let spritemap = 
    System.IO.File.ReadAllLines "./content/spritemap.txt"
    |> Array.map (fun line -> 
        let sa = line.Split ([|'\t';','|], System.StringSplitOptions.RemoveEmptyEntries)
        sa.[0], (int sa.[1], int sa.[2], int sa.[3], int sa.[4]))
    |> Map.ofArray


let sprite (sw, sh, sx, sy) (w, h) (x, y) colour =
    OnDraw (fun loadedAssets _ (spriteBatch: SpriteBatch) ->
        let texture = loadedAssets.textures.["sprites"]
        spriteBatch.Draw (texture, rect x y w h, System.Nullable(rect sx sy sw sh), colour))