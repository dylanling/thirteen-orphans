module TileTests exposing ( intRankTest )

import Test exposing (..)
import Expect

import Tile exposing (..)

intRankTest : Test
intRankTest =
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