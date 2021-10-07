module Main exposing (..)
import Browser
import Html exposing (Html, div, text, span)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Random
import Random.Set
-- TYPES
type GameStatus = 
  NotStart |
  Playing |
  Finished

type alias Cell = { covered: Bool, mine: Bool }
type alias Row = List Cell
type alias Grid = List Row
type alias Model = { grid: Grid }

type alias BombPositions = Set (Int, Int)
type Msg 
  = OpenCell { x: Int, y: Int }
  | FlagCell { x: Int, y: Int }
  | AddBombs BombPositions


-- STATE UPDATE HANDLERS

openCellOnGridLocation: Grid -> Int -> Int -> Grid
openCellOnGridLocation grid x y =
  List.indexedMap (\i row -> ( List.indexedMap (\j cell -> if (i == x && j == y && cell.covered) then { cell | covered = False} else cell) row)) grid

placeBombOnGridLocation: Grid -> Int -> Int -> Grid
placeBombOnGridLocation grid x y =
  List.indexedMap (\i row -> ( List.indexedMap (\j cell -> if (i == x && j == y) then { cell | mine = True} else cell) row)) grid


update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
     OpenCell cell -> ({model | grid = (openCellOnGridLocation model.grid cell.x cell.y)}, Cmd.none)
     AddBombs bombs -> 
      let
        newGrid = Set.foldl (\(x, y) grid -> (placeBombOnGridLocation grid x y)) model.grid bombs
        newModel = { model | grid = newGrid }
      in (newModel, Cmd.none)
     _ -> (model, Cmd.none)


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
displayGrid grid = div [] (List.indexedMap displayRow grid)

view: Model -> Html Msg
view model = 
  div []
  [
    text "Minesweeper",
    displayGrid model.grid
  ]


-- INITIAL STATE

getRandomBombPositions: Int -> Int -> Int -> Random.Generator BombPositions
getRandomBombPositions count rows columns = 
  Random.Set.set count (Random.pair (Random.int 0 rows) (Random.int 0 columns))


setBombOnGridLocation: Grid -> Int -> Int -> Grid
setBombOnGridLocation grid x y =
  List.indexedMap (\i row -> ( List.indexedMap (\j cell -> if (i == x && j == y) then { cell | mine = True} else cell) row)) grid

initialGrid: Int -> Int -> List (List Cell)
initialGrid rows columns = 
  List.repeat rows (List.repeat columns (Cell False False) )

initialCommand = Random.generate AddBombs (getRandomBombPositions 10 10 10)

init : () -> (Model, Cmd Msg)
init _ = (Model (initialGrid 10 10), initialCommand)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}
