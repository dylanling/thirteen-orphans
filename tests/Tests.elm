module Tests exposing (..)

import Test exposing (..)
import Expect

import Tile exposing (..)
import Suit exposing (..)
import Meld exposing (..)
import EveryDict exposing (..)

-- all : Test
-- all =
--   Test.concat [suitMatching, melds]

melds : Test
melds = 
  describe "Tests for tile melds"
    [ test "pong" <|
        \_ -> meld [Red, Red, Red] |> Expect.equal (Pong Red)
    , test "kong" <|
        \_ -> meld [East, East, East, East] |> Expect.equal (Kong East)
    , test "eyes" <|
        \_ -> meld [Circle Two, Circle Two] |> Expect.equal (Eyes (Circle Two))
    , test "chow" <|
        \_ -> meld [Bamboo Two, Bamboo Four, Bamboo Three] |> Expect.equal (Chow (Bamboo Two) (Bamboo Four) (Bamboo Three))
    , test "invalid 1" <|
        \_ -> meld [Bamboo Two, Bamboo Four, Bamboo Five] |> Expect.equal Invalid
    , test "invalid 2" <|
        \_ -> meld [Red, Red, Red, Red, Red] |> Expect.equal Invalid
    , test "invalid 3" <|
        \_ -> meld [Red, White] |> Expect.equal Invalid
    , test "invalid 4" <|
        \_ -> meld [Red, Bamboo Three, East] |> Expect.equal Invalid
    , test "invalid 5" <|
        \_ -> meld [East, North] |> Expect.equal Invalid
    , test "invalid 6" <|
        \_ -> meld [Bamboo Two, Bamboo Four, Character Three] |> Expect.equal Invalid
    ]

