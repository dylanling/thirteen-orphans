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