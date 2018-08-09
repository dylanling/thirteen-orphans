module Meld exposing (..)

import Tile exposing (..)

type Meld = 
    Eyes Tile 
  | Pong Tile
  | Kong Tile
  | Chow Tile Tile Tile
  | Invalid

meld : List Tile -> Meld
meld tiles =
  case tiles of
    [a, b] ->
      toMeld (allEqualTo a) tiles (Eyes a)
    [a, b, c] ->
      let
        pong = toMeld (allEqualTo a) tiles (Pong a)
      in
        case pong of
          Pong _ -> 
            pong
          _ ->
            toMeld isChow tiles (Chow a b c)
    [a, b, c, d] ->
      toMeld (allEqualTo a) tiles (Kong a)
    _ -> 
      Invalid

toMeld : (List Tile -> Bool) -> List Tile -> Meld -> Meld
toMeld predicate list meld =
  if (predicate list) then meld else Invalid

allEqualTo : Tile -> List Tile -> Bool
allEqualTo tile tiles =
  case (List.head tiles, List.tail tiles) of
    (Just head, Just tail) -> head == tile && allEqualTo tile tail
    (Just head, Nothing) -> head == tile
    _ -> True

isChow : List Tile -> Bool
isChow tiles = 
  case tiles of
    [Bamboo a, Bamboo b, Bamboo c] -> 
      sequential (int a) (int b) (int c)
    [Circle a, Circle b, Circle c] -> 
      sequential (int a) (int b) (int c)
    [Character a, Character b, Character c] -> 
      sequential (int a) (int b) (int c)
    _ -> False

sequential : Int -> Int -> Int -> Bool
sequential a b c =
  let
    lowest = min a (min b c)
    seq = \list -> list == [0, 1, 2]
  in
    List.sort [a, b, c] 
      |> List.map (\x -> x - lowest)
      |> seq

sameSimple : Tile -> Tile -> Bool
sameSimple a b =
  case (a, b) of
    (Bamboo _, Bamboo _) -> True
    (Circle _, Circle _) -> True
    (Character _, Character _) -> True
    _ -> False