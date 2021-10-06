module Main exposing (..)
import Browser
import Html exposing (Html, div, text, span)

type GameStatus = 
  NotStart |
  Playing |
  Finished

type alias Cell = { covered: Bool, mine: Bool }
  
type alias Model = { grid: List (List Cell) }

type Msg = 
  OpenCell { x: Int, y: Int } |
  FlagCell { x: Int, y: Int }

update: Msg -> Model -> Model
update _ model = 
  model

displayCell: Cell -> Html Msg
displayCell { covered, mine } =

  case (covered, mine) of
    (True, _) -> span [] [text "📦"]
    (False, True) -> span [] [text "💣"]
    (False, False) -> span [] [text "8"] -- calculate and put actual number

displayRow: List Cell -> Html Msg
displayRow row =
  div [] (List.map displayCell row)

displayGrid: List (List Cell) -> Html Msg
displayGrid grid =
  div [] (List.map displayRow grid)

view: Model -> Html Msg
view model =
  div []
  [
    text "Minesweeper",
    displayGrid model.grid
  ]

initialGrid: Int -> Int -> List (List Cell)
initialGrid rows columns = List.repeat rows (List.repeat columns (Cell True False) )

initialModel : Model
initialModel = Model (initialGrid 10 10)
main = Browser.sandbox { init = initialModel, update = update, view = view}
