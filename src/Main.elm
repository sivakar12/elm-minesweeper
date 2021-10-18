module Main exposing (..)
import Browser

import Types exposing (..)
import StateUpdate exposing 
  ( update
  , addRandomBombsCommand
  , createEmptyGrid
  , getBombCountFromGridSize
  )
import ViewElmUI exposing (view)

initialWidth : Int
initialWidth =
  6

initialHeight : Int
initialHeight =
  10

initialBombs : Int
initialBombs = 
  getBombCountFromGridSize initialWidth initialHeight

initialCommand : Cmd Msg
initialCommand =
  addRandomBombsCommand initialWidth initialHeight initialBombs

init : () -> (Model, Cmd Msg)
init _ = 
  (
    { grid = createEmptyGrid initialWidth initialHeight
    , flaggingMode = False
    , gameState = NotStarted
    },
    initialCommand
  )

subscriptions : Model -> Sub Msg
subscriptions _ = 
  Sub.none

main : Program () Model Msg
main = 
  Browser.element 
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }