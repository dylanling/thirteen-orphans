module Tile exposing (..)

import Utilities exposing (..)
import EveryDict

type Rank =
    One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine

type Tile = 
    Bamboo Rank 
  | Circle Rank
  | Character Rank
  | Red
  | Green
  | White
  | East
  | South
  | West
  | North

type Suit =
    Dragons
  | Winds
  | Bamboos
  | Circles
  | Characters

int : Rank -> Int
int rank =
  case rank of 
    One -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9

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
