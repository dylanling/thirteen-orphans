module CoreTests exposing (..)

import Test exposing (..)

import TileTests exposing ( intRankTest )
import MeldTests exposing ( meldTest, meldableGroupsTest )

suite : Test
suite =
  Test.concat [intRankTest, meldTest, meldableGroupsTest]