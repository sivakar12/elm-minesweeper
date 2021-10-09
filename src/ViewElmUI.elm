module ViewElmUI exposing (view)

import Element as E
import Element.Font
import Element.Input
import Element.Events
import Element.Border
import Element.Background
import Element.Cursor
import Html exposing (Html)
import Grid exposing (Grid)
import Array

import Types exposing (..)

cellStyles =
  [ E.width <| E.fillPortion 1
  , E.height <| E.fillPortion 1
  -- , Element.Border.width 1
  , Element.Cursor.pointer
  , Element.Font.size 40
  ]

numbersToEmoji = 
  Array.fromList 
    ["⬜", "1️⃣", "2️⃣", "3️⃣", "4️⃣", "5️⃣", "6️⃣", "7️⃣", "8️⃣", "9️⃣"]

displayCell: Int -> Int -> Cell -> E.Element Msg
displayCell x y { covered, mine, neighboringBombs } =
  case (covered, mine) of
    (Covered, _) -> E.el (cellStyles ++ [Element.Events.onClick <| HandleCellClick { x = x, y = y}]) <| E.text "📦"
    (Opened, Mined) -> E.el cellStyles <| E.text "💣"
    (Opened, NotMined) -> E.el cellStyles <| E.text <| Maybe.withDefault "" <| Array.get neighboringBombs numbersToEmoji
    (Flagged, _) -> E.el (cellStyles ++ [Element.Events.onClick <| HandleCellClick { x = x, y = y}]) <| E.text "⛳"

displayRow: Int -> Array.Array Cell -> E.Element Msg
displayRow y row =
  E.row
    [E.padding 2, E.spacing 5]
    <| Array.toList <| Array.indexedMap (\x cell -> (displayCell x y cell)) <| row

gridView: Grid Cell -> E.Element Msg
gridView grid =
  E.column
    [E.padding 2, E.spacing 10]
    <| Array.toList <| Array.indexedMap displayRow <| Grid.rows grid

controlsView: E.Element Msg
controlsView =
  let
    controlButtonStyles =
      [ Element.Border.width 1
      -- , Element.Border.rounded 50
      ]
  in
  E.row
    []
    [
      E.text "Width: ",
      Element.Input.button controlButtonStyles { label = E.text "➕", onPress = Maybe.Just <| ChangeGridSize Width Increase },
      Element.Input.button controlButtonStyles { label = E.text "➖", onPress = Maybe.Just <| ChangeGridSize Width Decrease },
      E.text "Height: ",
      Element.Input.button controlButtonStyles { label = E.text "➕", onPress = Maybe.Just <| ChangeGridSize Height Increase },
      Element.Input.button controlButtonStyles { label = E.text "➖", onPress = Maybe.Just <| ChangeGridSize Height Decrease }, 
      Element.Input.button controlButtonStyles { label = E.text "Start", onPress = Maybe.Just StartGame}
    ]

-- Todo
-- splashScreen: Model -> Html Msg
-- helpScreen: Model -> Html Msg
bottomDisplay: GameState -> Bool -> E.Element Msg
bottomDisplay gameState flaggingMode =
  case gameState of
    NotStarted -> controlsView
    Playing -> flagToggle flaggingMode
    Finished isWon ->
      case isWon of
        True -> E.text "Won"
        False -> E.text "Lost"

flagToggle: Bool -> E.Element Msg
flagToggle flaggingMode =
  let
    redBackground = 
      [
        Element.Background.color <| E.rgb 0 0 0,
        Element.Font.color <| E.rgb 255 255 255
      ]
    whiteBackground = 
      [
        Element.Font.color <| E.rgb 0 0 0,
        Element.Background.color <| E.rgb 255 255 255
      ]
    (openModeText, flagModeText) =
      case flaggingMode of
        True -> (E.el whiteBackground (E.text "OPEN"), E.el redBackground (E.text "FLAG"))
        False -> (E.el redBackground (E.text "OPEN"), E.el whiteBackground (E.text "FLAG"))
  in
  E.el 
    [
      Element.Events.onClick ToggleFlaggingMode
      -- E.padding 20, E.spacing 5
    ]
    (E.row [E.spacing 20] [openModeText, flagModeText])
    -- (E.text <| String.concat ["🚩",(if flaggingMode then "✔️" else "❌")])
view: Model -> Html Msg
view model = 
  E.layout 
    [
      E.height E.fill,
      E.width E.fill
    ] 
    <| E.column
      [
          E.centerX,
          E.centerY,
          E.height E.fill
      ] 
      [
        E.el [E.alignTop, E.centerX, Element.Font.size 50] <| E.text "Minesweeper",
        E.el [E.centerX, E.centerY] <| gridView model.grid,
        E.el [E.alignBottom, E.centerX, E.padding 20] <| bottomDisplay model.gameState model.flaggingMode
      ]
