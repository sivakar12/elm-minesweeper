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
  , Element.Cursor.pointer
  , Element.Font.size 40
  ]


numbersToEmoji = 
  Array.fromList 
    [ "⬜", "1️⃣", "2️⃣", "3️⃣", "4️⃣", "5️⃣", "6️⃣", "7️⃣", "8️⃣", "9️⃣" ]


displayCell: Int -> Int -> Cell -> E.Element Msg
displayCell x y { covered, mine, neighboringBombs } =
  case (covered, mine) of
    (Covered, _) ->
      E.el 
        (
          cellStyles ++ 
          [
            Element.Events.onClick 
            <| HandleCellClick { x = x, y = y }
          ]
        ) 
        <| E.text "📦"
    
    (Opened, Mined) ->
      E.el 
        cellStyles 
        (E.text "💣")
    
    (Opened, NotMined) -> 
      E.el
        cellStyles
        (
          E.text 
          <| Maybe.withDefault "" 
          <| Array.get neighboringBombs numbersToEmoji
        )
    
    (Flagged, _) ->
      E.el 
        (
          cellStyles ++ 
          [
            Element.Events.onClick 
            <| HandleCellClick { x = x, y = y }
          ]
        )
        (E.text "⛳")


displayRow: Int -> Array.Array Cell -> E.Element Msg
displayRow y row =
  E.row
    [ E.padding 2, E.spacing 5 ]
    (
      row
      |> Array.indexedMap (\x cell -> (displayCell x y cell))
      |> Array.toList 
    )


gridView: Grid Cell -> E.Element Msg
gridView grid =
  E.column
    [ E.padding 2, E.spacing 10 ]
    (
      Grid.rows grid
      |> Array.indexedMap displayRow
      |> Array.toList
    )


controlsView: E.Element Msg
controlsView =
  let
    controlButtonStyles =
      []
  in
  E.row
    [ E.spacing 10 ]
    [ E.text "Width: "
    , Element.Input.button 
        controlButtonStyles 
        { label = E.text "➕"
        , onPress = Maybe.Just <| ChangeGridSize Width Increase 
        }
    , Element.Input.button 
        controlButtonStyles 
        { label = E.text "➖"
        , onPress = Maybe.Just <| ChangeGridSize Width Decrease
        }
    , E.text "Height: "
    , Element.Input.button 
        controlButtonStyles
        { label = E.text "➕"
        , onPress = Maybe.Just <| ChangeGridSize Height Increase 
        }
    , Element.Input.button 
        controlButtonStyles 
        { label = E.text "➖"
        , onPress = Maybe.Just <| ChangeGridSize Height Decrease
        }
    , Element.Input.button 
        [Element.Font.heavy]
        { label = E.text "Start"
        , onPress = Maybe.Just StartGame
        }
    ]


flagToggle: Bool -> E.Element Msg
flagToggle flaggingMode =
  let
    flaggedStyle = 
      [
        Element.Background.color <| E.rgb 255 0 0,
        Element.Font.color <| E.rgb 255 255 255
      ]
    nonFlaggedStyle = []
    chosenStyle = 
      if flaggingMode then flaggedStyle else nonFlaggedStyle
  in
  E.el 
    (chosenStyle ++ [ Element.Events.onClick ToggleFlaggingMode ])    
    (E.text "🚩")


displayResult: Bool -> E.Element Msg
displayResult isWon =
  let 
    resultText =
      case isWon of
        True -> 
          "YOU WON! 🎉"
        False ->
          "YOU LOSE!!"
    resultStyle =
      [
        Element.Background.color <| E.rgb 255 0 0,
        Element.Font.color <| E.rgb 0 0 0,
        Element.Font.heavy
      ]
  in
  E.row
    [ E.spacing 20 ]
    [
      E.el resultStyle <| E.text resultText,
      Element.Input.button 
        []
        { label = E.text "New Game", onPress = Maybe.Just StartGame }
    ]


bottomDisplay: GameState -> Bool -> E.Element Msg
bottomDisplay gameState flaggingMode =
  case gameState of
    NotStarted -> 
      controlsView
    Playing -> 
      flagToggle flaggingMode
    Finished isWon ->
      displayResult isWon



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
          E.height E.fill,
          Element.Font.family [Element.Font.monospace]
      ] 
      [
        E.el 
          [
            E.alignTop, E.centerX, 
            Element.Font.size 40, 
            E.padding 20
          ] 
          (E.text "MINESWEEPER"),
        E.el
          [E.centerX, E.centerY]
          (gridView model.grid),
        E.el 
          [E.alignBottom, E.centerX, E.padding 20]
          (bottomDisplay model.gameState model.flaggingMode)
      ]
