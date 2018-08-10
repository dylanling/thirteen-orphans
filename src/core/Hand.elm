module Hand exposing (..)

import Tile exposing (..)
import Meld exposing (..)

type alias Hand =
  { closed : List Tile
  , open : List Meld
  }

melds : Hand -> List (List Meld)
melds hand =
  []