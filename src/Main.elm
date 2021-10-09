module Main exposing (..)
import Maybe
import Array
import Set exposing (Set)
import Browser
import Html exposing (Html)
import Random
import Random.Set
import Grid exposing (Grid)


import Types exposing (..)
import StateUpdate exposing 
  ( update
  , addRandomBombsCommand
  , createEmptyGrid
  , getBombCountFromGridSize
  )
import ViewElmUI exposing (view)

initialWidth =
  6
initialHeight =
  10
initialBombs = 
  getBombCountFromGridSize initialWidth initialHeight

initialCommand =
  addRandomBombsCommand

init : () -> (Model, Cmd Msg)
init _ = 
  (
    { grid = createEmptyGrid initialWidth initialHeight
    , flaggingMode = False
    , gameState = NotStarted
    },
    initialCommand initialWidth initialHeight initialBombs
  )

subscriptions : Model -> Sub Msg
subscriptions _ = 
  Sub.none

main = 
  Browser.element 
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }