module TileTests exposing (..)

import Test exposing (..)
import Expect

all : Test
all =
  describe "Tile Test Suite"
    [
      test "sample test" <|
        \_ -> True |> Expect.equal False
    ]