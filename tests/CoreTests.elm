module CoreTests exposing (..)

import Test exposing (..)

import TileTests exposing ( intRankTest )
import MeldTests exposing ( meldTest, meldsTest, meldableGroupsTest )

suite : Test
suite =
  Test.concat [intRankTest, meldTest, meldsTest, meldableGroupsTest]