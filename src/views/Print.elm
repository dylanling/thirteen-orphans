module Print exposing (..)

import Tile exposing (..)
import Meld exposing (..)
import TileOrdering exposing (tileOrdering)

displayTiles : (Tile -> String) -> String -> List Tile -> String
displayTiles show delimiter tiles =
  tiles 
    |> List.map show
    |> String.join delimiter

displaySortedTiles : (Tile -> String) -> String -> List Tile -> String
displaySortedTiles show delimiter tiles = 
  displayTiles show delimiter (List.sortWith tileOrdering tiles)

displayMeld : (Tile -> String) -> String -> Meld -> String
displayMeld show delimiter meld =
  case meld of
    Chow a b c -> displayTiles show delimiter [a,b,c]
    Eyes tile -> displayTiles show delimiter [tile, tile]
    Pong tile -> displayTiles show delimiter [tile, tile, tile]
    Kong tile -> displayTiles show delimiter [tile, tile, tile, tile]
    _ -> ""

displayMeldsAsEmojis : List Meld -> String
displayMeldsAsEmojis melds =
  List.map (displayMeld emoji " ") melds
    |> String.join ","

emojis : List Tile -> String
emojis tiles =
  displaySortedTiles emoji " " tiles
  
displayTile : Tile -> String
displayTile tile =
  case tile of
    Bamboo rank -> (Basics.toString rank) ++ " of Bamboos"
    Circle rank -> (Basics.toString rank) ++ " of Circles"
    Character rank -> (Basics.toString rank) ++ " of Characters"
    _ -> Basics.toString tile

emoji : Tile -> String
emoji tile =
  case tile of
    Bamboo One -> "🀐"
    Bamboo Two -> "🀑"
    Bamboo Three -> "🀒"
    Bamboo Four -> "🀓"
    Bamboo Five -> "🀔"
    Bamboo Six -> "🀕"
    Bamboo Seven -> "🀖"
    Bamboo Eight -> "🀗"
    Bamboo Nine -> "🀘"
    Circle One -> "🀙"
    Circle Two -> "🀚"
    Circle Three -> "🀛"
    Circle Four -> "🀜"
    Circle Five -> "🀝"
    Circle Six -> "🀞"
    Circle Seven -> "🀟"
    Circle Eight -> "🀠"
    Circle Nine -> "🀡"
    Character One -> "🀇"
    Character Two -> "🀈"
    Character Three -> "🀉"
    Character Four -> "🀊"
    Character Five -> "🀋"
    Character Six -> "🀌"
    Character Seven -> "🀍"
    Character Eight -> "🀎"
    Character Nine -> "🀏"
    Red -> "🀄"
    Green -> "🀅"
    White -> "🀆"
    East -> "🀀"
    South -> "🀁"
    West -> "🀂"
    North -> "🀃"