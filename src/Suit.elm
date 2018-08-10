module Suit exposing (..)

import Utilities exposing (..)
import Tile exposing (..)
import EveryDict

type Suit =
    Dragons
  | Winds
  | Bamboos
  | Circles
  | Characters

suit : Tile -> Suit
suit tile =
  case tile of
    Bamboo _ -> Bamboos
    Circle _ -> Circles
    Character _ -> Characters
    Red -> Dragons
    Green -> Dragons
    White -> Dragons
    East -> Winds
    South -> Winds
    West -> Winds
    North -> Winds

groupBySuits : List Tile -> EveryDict.EveryDict Suit (List Tile)
groupBySuits tiles = 
  List.map (\tile -> (suit tile, tile)) tiles
    |> List.foldl 
      (\(s, t) dict -> EveryDict.update 
        s 
        (insertOrAppend t)
        dict)
      EveryDict.empty
