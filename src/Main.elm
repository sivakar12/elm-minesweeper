module Main exposing (..)
import Browser
import Html exposing (Html, div, text, span)
import Html.Events exposing (onClick)
import Set

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

getRandomBombPositions: Int -> Int -> Int -> List (Int, Int)
getRandomBombPositions rows columns bombsCount =
  [(0, 0), (1, 1), (2, 2), (3,3), (4,4), (5,5), (6,6), (7, 7)]

setBombOnGridLocation: Grid -> Int -> Int -> Grid
setBombOnGridLocation grid x y =
  List.indexedMap (\i row -> ( List.indexedMap (\j cell -> if (i == x && j == y) then { cell | mine = True} else cell) row)) grid

initialGrid: Int -> Int -> List (List Cell)
initialGrid rows columns = 
  let
    bombPositions: List (Int, Int)
    bombPositions = getRandomBombPositions rows columns 10

    emptyGrid = List.repeat rows (List.repeat columns (Cell True False) )

    bombedGrid = List.foldl (\(x, y) grid -> setBombOnGridLocation grid x y) emptyGrid bombPositions
  in
    bombedGrid
initialModel : Model
initialModel = Model (initialGrid 10 10)

-- START
main = Browser.sandbox { init = initialModel, update = update, view = view}
