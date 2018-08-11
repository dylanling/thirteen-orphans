module Meld exposing (..)

import EveryDict
import List.Unique
import Ordering exposing ( Ordering )

import Tile exposing (..)
import Suit exposing (..)
import TileOrdering exposing ( tileOrdering )
import Utilities exposing (..)

type Meld = 
    Eyes Tile 
  | Pong Tile
  | Kong Tile
  | Chow Tile Tile Tile
  | Invalid

melds : List Tile -> List (List Meld)
melds tiles =
  meldableGroups tiles
    |> List.map meldsInGroup
    |> cartesianProduct
    |> List.map flatten

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
            toMeld isChow tiles (normalize (Chow a b c))
    [a, b, c, d] ->
      toMeld (allEqualTo a) tiles (Kong a)
    _ -> 
      Invalid

-- given a list of tiles within the same meldable group
-- return all legal partitions of the list such that each
-- partition can be evaluated as a meld
-- 
-- while branching we need to aggressively prune any
-- time we've selected a partition that cannot be evaluated
-- as a meld given that Bell numbers grow too quickly to
-- possibly evaluate all partitions
--
-- we should also quickly terminate for any list 
-- of non-simples.
meldsInGroup : List Tile -> List (List Meld)
meldsInGroup tiles =
  case tiles of
    [] -> 
      []
    first::_ -> 
      let
        tileMelds = 
          if isHonor (suit first) 
          then meldsForHonors tiles
           else meldsForSimples tiles
      in
        tileMelds 
          |> List.map (List.map meld)
          |> List.map (List.sortWith meldOrdering)
          |> List.Unique.fromList
          |> List.Unique.toList

-- we're cheating a bit here because a set of some honor
-- can only ever have one melded value given that there are
-- at most four of any individual honor. otherwise we'd
-- have to do some make coin change dynamic programming nonsense
meldsForHonors : List Tile -> List (List (List Tile))
meldsForHonors honors =
  if List.member (List.length honors) [2, 3, 4]
  then [[honors]]
  else []

meldsForSimples : List Tile -> List (List (List Tile))
meldsForSimples simples = 
  let
    optimizedBloat = \tile melds -> bloat tile melds |> List.filter allCanBecomeValidMelds
    applyBloat = \n l -> l |> List.map (optimizedBloat n) |> flatten
  in  
    List.foldl applyBloat [[]] simples
      |> List.filter allValidMelds

allValidMelds : List (List Tile) -> Bool
allValidMelds melds =
  case melds of
    [] -> 
      True
    x::xs -> 
      if meld x == Invalid 
      then False
      else allValidMelds xs

allCanBecomeValidMelds : List (List Tile) -> Bool
allCanBecomeValidMelds melds =
  case melds of
    [] -> True
    x::xs -> canBecomeValidMeld x && allCanBecomeValidMelds xs

canBecomeValidMeld : List Tile -> Bool
canBecomeValidMeld tiles =
  case tiles of
    [] -> True
    x::xs -> canJoinMeld x xs

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

normalize : Meld -> Meld
normalize meld = 
  case meld of
    Chow a b c -> 
      case List.sortWith tileOrdering [a, b, c] of
        [x, y, z] -> Chow x y z
        _ -> meld
    _ -> 
      meld


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

meldOrdering : Ordering Meld
meldOrdering = 
  Ordering.byFieldWith tileOrdering firstTileOf
    |> Ordering.breakTiesWith (Ordering.byField basicMeldOrder)

basicMeldOrder : Meld -> Int
basicMeldOrder meld =
  case meld of
    Eyes _ -> 0
    Pong _ -> 1
    Kong _ -> 2
    Chow _ _ _ -> 3
    Invalid -> 4

firstTileOf : Meld -> Tile
firstTileOf meld = 
  case meld of
    Chow tile _ _ -> tile
    Eyes tile -> tile
    Pong tile -> tile 
    Kong tile -> tile
    _ -> North -- shouldn't happen, but just in case, North is the last tile by order