module Meld exposing (..)

import Tile exposing (..)
import Suit exposing (..)
import Utilities exposing (..)
import EveryDict

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

-- given a list of tiles within the same meldable group
-- return all legal partitions of the list such that each
-- partition can be evaluated as a meld
-- 
-- while recursing we need to aggressively terminate any
-- time we've selected a partition that cannot be evaluated
-- as a meld given that Bell numbers grow too quickly to
-- possibly evaluate all partitions

-- we should also quickly terminate for any list 
-- of non-simples.
melds : List Tile -> List (List Tile)
melds tiles =
  case tiles of
    [] -> 
      []
    first::_ -> 
      if isHonor (suit first) 
      then meldsForHonors tiles 
      else meldsForSimples tiles

-- we're cheating a bit here because a set of some honor
-- can only ever have one melded value given that there are
-- at most four of any individual honor. otherwise we'd
-- have to do some make coin change dynamic programming nonsense
meldsForHonors : List Tile -> List (List Tile)
meldsForHonors honors =
  if List.member (List.length honors) [2, 3, 4]
  then [honors]
  else []

meldsForSimples : List Tile -> List (List Tile)
meldsForSimples simples = 
  if List.length simples < 5
  then 
    case meld simples of
      Invalid -> []
      _ -> [simples]
  else 
    []

meldsForSimplesStartingWith : List Tile -> List Tile -> List Tile -> List (List Tile)
meldsForSimplesStartingWith current all tiles =
  case current of
    [] -> 
      []
    _ -> 
      []

meldsWithTileAdded : Tile -> List (List Tile) -> List (List Tile)

canJoinMeld : Tile -> List Tile -> Bool
canJoinMeld tile tiles =
  case tiles of
    [] -> True
    [a] -> canBeInSameMeld tile a
    _ -> not (meld ([tile] ++ tiles) == Invalid)

canBeInSameMeld : Tile -> Tile -> Bool
canBeInSameMeld a b =
  let 
    ranksInRange = \s t -> abs (s - t) < 3
  in
    case (a, b) of
      (Bamboo x, Bamboo y) -> 
        ranksInRange (int x) (int y)
      (Circle x, Circle y) -> 
        ranksInRange (int x) (int y)
      (Character x, Character y) -> 
        ranksInRange (int x) (int y)
      _ ->
        a == b

meldableGroups : List Tile -> List (List Tile)
meldableGroups tiles =
  [Winds, Dragons, Characters, Circles, Bamboos]
    |> List.map (\suit -> groupedBySuitEntry suit (groupBySuits tiles))
    |> List.foldl (++) []
    |> filterNot List.isEmpty

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
  in
    List.sort [a, b, c] 
      |> List.map (\x -> x - lowest)
      |> (==) [0, 1, 2]

sameSimple : Tile -> Tile -> Bool
sameSimple a b =
  case (a, b) of
    (Bamboo _, Bamboo _) -> True
    (Circle _, Circle _) -> True
    (Character _, Character _) -> True
    _ -> False

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
      grouped (getSuitTiles suit dict)
    Winds ->
      grouped (getSuitTiles suit dict)

getSuitTiles : Suit -> EveryDict.EveryDict Suit (List Tile) -> List Tile
getSuitTiles suit dict =
  case EveryDict.get suit dict of
    Just tiles -> tiles
    Nothing -> []