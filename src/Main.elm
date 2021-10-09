module Main exposing (..)
import Browser
import Set exposing (Set)
import Html exposing (Html)
import Random
import Random.Set
import Grid exposing (Grid)

import Maybe
import Array

import Types exposing (..)
import StateUpdate exposing (update, addRandomBombsCommand, createEmptyGrid)
import ViewElmUI exposing (view)


-- INITIAL STATE
initialWidth = 6
initialHeight = 10
initialBombs = 20




initialCommand = addRandomBombsCommand

init : () -> (Model, Cmd Msg)
init _ = 
  (
    Model <|createEmptyGrid initialWidth initialHeight, 
    initialCommand initialWidth initialHeight initialBombs
  )

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}
