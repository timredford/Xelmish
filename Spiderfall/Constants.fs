module Constants
open Xelmish.Model
open Xelmish.Viewables

let fontForegroundColor = Colour.LightGray
let backgroundColor = Colour.Black

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

let button s event background foreground (width, height) (x, y) fontsize = 
    [
        colour background (width, height) (x, y)
        text primaryFontName fontsize foreground (-0.5, -0.5) s (x + width/2, y+height/2)
        onclick event (width, height) (x, y)
    ]

let grayButton s event (width, height) (x, y) fontsize = 
    button s event Colour.Gray Colour.White (width, height) (x,y) fontsize

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