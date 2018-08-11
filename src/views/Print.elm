module Print exposing (..)

import Tile exposing (..)
import TileOrdering exposing (tileOrdering)

display : (Tile -> String) -> String -> List Tile -> String
display show delimiter tiles =
  tiles 
    |> List.map show
    |> String.join delimiter

displaySorted : (Tile -> String) -> String -> List Tile -> String
displaySorted show delimiter tiles = 
  display show delimiter (List.sortWith tileOrdering tiles)

emojis : List Tile -> String
emojis tiles =
  displaySorted emoji " " tiles

show : Tile -> String
show tile =
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