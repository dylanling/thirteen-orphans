module Tile exposing (..)

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

int : Rank -> Int
int r =
  case r of 
    One -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9

allTiles : List Tile
allTiles = 
  [ Bamboo One
  , Bamboo Two
  , Bamboo Three
  , Bamboo Four
  , Bamboo Five
  , Bamboo Six
  , Bamboo Seven
  , Bamboo Eight
  , Bamboo Nine
  , Circle One
  , Circle Two
  , Circle Three
  , Circle Four
  , Circle Five
  , Circle Six
  , Circle Seven
  , Circle Eight
  , Circle Nine
  , Character One
  , Character Two
  , Character Three
  , Character Four
  , Character Five
  , Character Six
  , Character Seven
  , Character Eight
  , Character Nine
  , Red
  , Green
  , White
  , East
  , South
  , West
  , North
  ]