module Suit exposing (..)

import EveryDict

import Tile exposing (..)
import Utilities exposing (..)

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

isHonor : Suit -> Bool
isHonor suit =
  case suit of
    Dragons -> True
    Winds -> True
    _ -> False

groupBySuits : List Tile -> EveryDict.EveryDict Suit (List Tile)
groupBySuits tiles = 
  List.map (\tile -> (suit tile, tile)) tiles
    |> List.foldl 
      (\(s, t) dict -> EveryDict.update 
        s 
        (insertOrAppend t)
        dict)
      EveryDict.empty
