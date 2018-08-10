module TileTests exposing (..)

import Test exposing (..)
import Expect

import Tile exposing (..)

tileRanks : Test
tileRanks =
  describe "Tile Rank Tests"
    [
      test "Rank 1 correspond to correct int" <|
        \_ -> int One |> Expect.equal 1
    , test "Rank 2 correspond to correct int" <|
        \_ -> int Two |> Expect.equal 2
    , test "Rank 3 correspond to correct int" <|
        \_ -> int Three |> Expect.equal 3
    , test "Rank 4 correspond to correct int" <|
        \_ -> int Four |> Expect.equal 4
    , test "Rank 5 correspond to correct int" <|
        \_ -> int Five |> Expect.equal 5
    , test "Rank 6 correspond to correct int" <|
        \_ -> int Six |> Expect.equal 6
    , test "Rank 7 correspond to correct int" <|
        \_ -> int Seven |> Expect.equal 7
    , test "Rank 8 correspond to correct int" <|
        \_ -> int Eight |> Expect.equal 8
    , test "Rank 9 correspond to correct int" <|
        \_ -> int Nine |> Expect.equal 9
    ]

-- melds : Test
-- melds = 
--   describe "Tests for tile melds"
--     [ test "pong" <|
--         \_ -> meld [Red, Red, Red] |> Expect.equal (Pong Red)
--     , test "kong" <|
--         \_ -> meld [East, East, East, East] |> Expect.equal (Kong East)
--     , test "eyes" <|
--         \_ -> meld [Circle Two, Circle Two] |> Expect.equal (Eyes (Circle Two))
--     , test "chow" <|
--         \_ -> meld [Bamboo Two, Bamboo Four, Bamboo Three] |> Expect.equal (Chow (Bamboo Two) (Bamboo Four) (Bamboo Three))
--     , test "invalid 1" <|
--         \_ -> meld [Bamboo Two, Bamboo Four, Bamboo Five] |> Expect.equal Invalid
--     , test "invalid 2" <|
--         \_ -> meld [Red, Red, Red, Red, Red] |> Expect.equal Invalid
--     , test "invalid 3" <|
--         \_ -> meld [Red, White] |> Expect.equal Invalid
--     , test "invalid 4" <|
--         \_ -> meld [Red, Bamboo Three, East] |> Expect.equal Invalid
--     , test "invalid 5" <|
--         \_ -> meld [East, North] |> Expect.equal Invalid
--     , test "invalid 6" <|
--         \_ -> meld [Bamboo Two, Bamboo Four, Character Three] |> Expect.equal Invalid
--     ]
