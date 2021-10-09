module ViewCss exposing (view)

import Types exposing (..)

import Html exposing (Html, div, span, text, input, button)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput, onDoubleClick)
import Grid exposing (Grid)
import Array

cellStyles = 
  [ style "aspect-ratio" "1"
  , style "font-size" "300%"
  , style "border" "2px solid black"
  , style "cursor" "pointer"
  , style "display" "flex"
  , style "justify-content" "center"
  , style "align-items" "center"
  , style "user-select" "none"
  ]


numbersToEmoji = 
  Array.fromList 
    ["⬜", "1️⃣", "2️⃣", "3️⃣", "4️⃣", "5️⃣", "6️⃣", "7️⃣", "8️⃣", "9️⃣"]

displayCell: Int -> Int -> Cell -> Html Msg
displayCell x y { covered, mine, neighboringBombs } =
  let
    clickHandlers = 
      [
        onClick (OpenCell{x = x, y = y}), 
        onDoubleClick (FlagCell { x = x, y = y})
      ]
  in
  case (covered, mine) of
    (Covered, _) -> span (cellStyles ++ clickHandlers) [text "📦"]
    (Opened, Mined) -> span cellStyles [text "💣"]
    (Opened, NotMined) -> span cellStyles [text <| Maybe.withDefault "" <| Array.get neighboringBombs numbersToEmoji]
    (Flagged, _) -> span cellStyles [text "🚩"]
displayGrid: Grid Cell -> Html Msg
displayGrid grid = 
  let
    height = Grid.height grid
    width = Grid.width grid
    dividor = min height width
    gridTemlateRows = String.concat [
      "repeat(",
      String.fromInt(height),
      ", ",
      -- String.fromFloat (80 / (toFloat dividor)),
      -- "vmin)"
      "auto)"
      ]
    gridTemplateColumns = String.concat [
      "repeat(",
      String.fromInt(width),
      ", ",
      -- String.fromFloat (80 / (toFloat dividor)),
      -- "vmin)"
      "auto)"
      ]
    gridOfDivs =  Grid.indexedMap displayCell grid
    flattenedDivs = Grid.foldl (\cell list -> List.append list [cell]) [] gridOfDivs
  in
  div 
    [ style "display" "grid"
    , style "grid-template-rows" gridTemlateRows -- replace with rows and vmin * 80 / rows
    , style "box-sizing" "border-box"
    , style "grid-template-columns" gridTemplateColumns -- replace with columns and vmin * 80 / columns
    , style "justify-content" "center"
    , style "align-content" "center"
    , style "align-items" "center"
    ] 
    flattenedDivs

header = 
  div 
    [
      style "font-size" "200%",
      style "margin-bottom" "2%"
    ] 
    [text "Minesweeper"]

controls =
  div
    [
      style "display" "flex",
      style "flex-direction" "row",
      style "justify-content" "space-around",
      style "margin-bottom" "1%"
    ]
    [
      span [] [text "Height: "],
      button [ onClick <| ChangeGridSize Height Increase ] [text "+"],
      button [ onClick <| ChangeGridSize Height Decrease ] [text "-"],
      span [] [text "Width: "],
      button [ onClick <| ChangeGridSize Width Increase ] [text "+"],
      button [ onClick <| ChangeGridSize Width Decrease ] [text "-"]
    ]
view: Model -> Html Msg
view model = 
  div 
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    , style "justify-content" "center"
    , style "justify-content" "space-between"
    , style "height" "100vh"
    ]
    [
      header,
      displayGrid model.grid,
      controls
    ]