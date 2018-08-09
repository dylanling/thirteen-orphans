module Tile exposing (..)

import EveryDict
import List.Extra

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

type Honor =
    Dragon
  | Wind

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

isSimple : Tile -> Bool
isSimple tile =
  case tile of
    Bamboo _ -> True
    Circle _ -> True
    Character _ -> True
    _ -> False

isDragon : Tile -> Bool
isDragon tile =
  case tile of
    Red -> True
    Green -> True
    White -> True
    _ -> False

isWind : Tile -> Bool
isWind tile =
  case tile of
    East -> True
    South -> True
    West -> True
    North -> True
    _ -> False

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

groupedByMeldable : List Tile -> List (List Tile)
groupedByMeldable tiles =
  let 
    grouped = groupBySuits tiles
  in
    EveryDict.values grouped

groupBySuits : List Tile -> EveryDict.EveryDict Suit (List Tile)
groupBySuits tiles = 
  List.map (\tile -> (suit tile, tile)) tiles
    |> List.foldl 
      (\(s, t) dict -> EveryDict.update 
        s 
        (insertOrAppend t)
        dict)
      EveryDict.empty

groupedBySuitEntry : Suit -> EveryDict.EveryDict Suit (List Tile) -> List (List Tile)
groupedBySuitEntry suit dict =
  case suit of
    Bamboos ->
      [getSuitTiles suit dict]
    Circles ->
      [getSuitTiles suit dict]
    Characters ->
      [getSuitTiles suit dict]
    Dragons ->
      []
    _ -> []

getSuitTiles : Suit -> EveryDict.EveryDict Suit (List Tile) -> List Tile
getSuitTiles suit dict =
  case EveryDict.get suit dict of
    Just tiles -> tiles
    Nothing -> []

insertOrAppend : a -> Maybe (List a) -> Maybe (List a)
insertOrAppend item m = 
  case m of
    Just list -> Just (list ++ [item])
    Nothing -> Just [item]