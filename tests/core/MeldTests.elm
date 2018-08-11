module MeldTests exposing ( meldTest, meldableGroupsTest )

import Test exposing (..)
import Expect

import Tile exposing (..)
import Meld exposing (..)

meldTest : Test
meldTest = 
  describe "Tests for tile melds"
    [ test "pong" <|
        \_ -> meld [Red, Red, Red] 
          |> Expect.equal (Pong Red)
    , test "kong" <|
        \_ -> meld [East, East, East, East] 
          |> Expect.equal (Kong East)
    , test "eyes" <|
        \_ -> meld [Circle Two, Circle Two] 
          |> Expect.equal (Eyes (Circle Two))
    , test "chow" <|
        \_ -> meld [Bamboo Two, Bamboo Four, Bamboo Three] 
          |> Expect.equal (Chow (Bamboo Two) (Bamboo Four) (Bamboo Three))
    , test "invalid 1" <|
        \_ -> meld [Bamboo Two, Bamboo Four, Bamboo Five] 
          |> Expect.equal Invalid
    , test "invalid 2" <|
        \_ -> meld [Red, Red, Red, Red, Red] 
          |> Expect.equal Invalid
    , test "invalid 3" <|
        \_ -> meld [Red, White] 
          |> Expect.equal Invalid
    , test "invalid 4" <|
        \_ -> meld [Red, Bamboo Three, East] 
          |> Expect.equal Invalid
    , test "invalid 5" <|
        \_ -> meld [East, North] 
          |> Expect.equal Invalid
    , test "invalid 6" <|
        \_ -> meld [Bamboo Two, Bamboo Four, Character Three] 
          |> Expect.equal Invalid
    ]

meldableGroupsTest : Test
meldableGroupsTest =
  describe "Tests for meldableGroups"
    [ test "simple same tile" <|
        \_ -> meldableGroups [Red, Red, Red] 
          |> Expect.equal [[Red, Red, Red]]
    , test "simple same tiles two suits" <|
        \_ -> meldableGroups [Red, Red, Red, East, East, East] 
          |> Expect.equal [[Red, Red, Red], [East, East, East]]
    , test "all groups" <|
        \_ -> meldableGroups 
          [ North
          , Circle Two
          , Bamboo Five
          , Red
          , Green
          , White
          , Red
          , East
          , West
          , Character Seven
          , North
          , Circle One
          , South
          ] 
          |> Expect.equal 
            [ [Bamboo Five]
            , [Circle Two, Circle One]
            , [Character Seven]
            , [Green]
            , [Red, Red]
            , [White]
            , [East]
            , [North, North]
            , [South]
            , [West]
            ]
    ]


