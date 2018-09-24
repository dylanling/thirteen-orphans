module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onInput )

import Tile exposing (..)
import Meld exposing (..)
import Suit exposing (..)
import Hand exposing (..)
import Print exposing (..)

---- MODEL ----


type alias Model = 
  { hand : Hand 
  , melds : List (List Meld)
  }

model : Model
model =
  { hand = { closed = [], open = [] }
  , melds = []
  }


init : ( Model, Cmd Msg )
init =
  ( model, Cmd.none )



---- UPDATE ----


type Msg
  = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
  div []
    [ img [ src "/logo.svg" ] []
    , h1 [] [ text "Your Elm App is working!" ]
    ]



---- PROGRAM ----


main : Program Never Model Msg
main =
  Html.program
    { view = view
    , init = init
    , update = update
    , subscriptions = always Sub.none
    }
