module Main exposing (..)
import Browser
import Html exposing (Html, div, text, span)
import Html.Events exposing (onClick)

-- TYPES
type GameStatus = 
  NotStart |
  Playing |
  Finished

type alias Cell = { covered: Bool, mine: Bool }
type alias Row = List Cell
type alias Grid = List Row
type alias Model = { grid: Grid }

type Msg = 
  OpenCell { x: Int, y: Int } |
  FlagCell { x: Int, y: Int }


-- STATE UPDATE HANDLERS

openCellOnGridLocation: Grid -> Int -> Int -> Grid
openCellOnGridLocation grid x y =
  List.indexedMap (\i row -> ( List.indexedMap (\j cell -> if (i == x && j == y && cell.covered) then { cell | covered = False} else cell) row)) grid
update: Msg -> Model -> Model
update msg model = 
  case msg of
     OpenCell cell -> {model | grid = (openCellOnGridLocation model.grid cell.x cell.y)}
     _ -> model


-- VIEWS
displayCell: Int -> Int -> Cell -> Html Msg
displayCell x y { covered, mine } =

  case (covered, mine) of
    (True, _) -> span [ onClick (OpenCell{x = x, y = y})] [text "📦"]
    (False, True) -> span [] [text "💣"]
    (False, False) -> span [] [text "8"] -- calculate and put actual number

displayRow: Int -> List Cell -> Html Msg
displayRow x row =
  div [] (List.indexedMap (\y cell -> displayCell x y cell) row)

displayGrid: List (List Cell) -> Html Msg
displayGrid grid =
  div [] (List.indexedMap displayRow grid)

view: Model -> Html Msg
view model =
  div []
  [
    text "Minesweeper",
    displayGrid model.grid
  ]


-- INITIAL STATE
initialGrid: Int -> Int -> List (List Cell)
initialGrid rows columns = List.repeat rows (List.repeat columns (Cell True False) )

initialModel : Model
initialModel = Model (initialGrid 10 10)

-- START
main = Browser.sandbox { init = initialModel, update = update, view = view}
