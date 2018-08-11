module TileOrdering exposing (..)

import Ordering exposing (Ordering)

import Tile exposing (..)
import Suit exposing (..)

tileOrdering : Ordering Tile
tileOrdering = 
  Ordering.byRank 
    suitInt 
    withinSuitOrdering

withinSuitOrdering : Tile -> Tile -> Order
withinSuitOrdering a b =
  case (a, b) of
    (Bamboo x, Bamboo y) -> rankOrdering x y
    (Circle x, Circle y) -> rankOrdering x y
    (Character x, Character y) -> rankOrdering x y
    _ -> 
      case (suit a, suit b) of
        (Dragons, Dragons) -> dragonsOrdering a b
        (Winds, Winds) -> windsOrdering a b
        _ -> Ordering.noConflicts

rankOrdering : Ordering Rank
rankOrdering = Ordering.byField int

suitInt : Tile -> Int
suitInt tile =
  case suit tile of
    Circles -> 0
    Bamboos -> 1
    Characters -> 2
    Dragons -> 3
    Winds -> 4

dragonsOrdering : Ordering Tile
dragonsOrdering = 
  Ordering.explicit [Red, Green, White]

windsOrdering : Ordering Tile
windsOrdering = 
  Ordering.explicit [East, South, West, North]