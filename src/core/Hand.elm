module Hand exposing (..)

import Tile exposing (..)
import Meld exposing (..)

type alias Hand =
  { closed : List Tile
  , open : List Meld
  }

handMelds : Hand -> List (List Meld)
handMelds hand =
  melds (.closed hand)
    |> List.map ((++) (.open hand))