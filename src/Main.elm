module Main exposing (..)
import Browser
import Set exposing (Set)
import Html exposing (Html)
import Random
import Random.Set
import Grid exposing (Grid)
import Element as E
import Element.Font
import Maybe

-- TYPES
type GameStatus = 
  NotStart |
  Playing |
  Finished

type CoverState 
  = Covered
  | Opened
  | Flagged

type MineState
  = Mined
  | NotMined

type alias Cell = { covered: CoverState, mine: MineState }
type alias Model = { grid: Grid Cell }

type alias BombPositions = Set (Int, Int)
type Msg 
  = OpenCell { x: Int, y: Int }
  | FlagCell { x: Int, y: Int }
  | AddBombs BombPositions
  | HandleRowInputChange String
  | HandleColumnInputChange String


-- STATE UPDATE HANDLERS
defaultCell = Cell Covered NotMined

openCellOnGridLocation: Grid Cell -> Int -> Int -> Grid Cell
openCellOnGridLocation grid x y =
  let
    oldCell = Maybe.withDefault defaultCell (Grid.get (x, y) grid)
  in
  Grid.set (x, y) { oldCell | covered = Opened } grid

flagCellOnGridLocation: Grid Cell -> Int -> Int -> Grid Cell
flagCellOnGridLocation grid x y =
  let
    oldCell = Maybe.withDefault defaultCell (Grid.get (x, y) grid)
  in
  Grid.set (x, y) { oldCell | covered = Flagged } grid

placeBombOnGridLocation: Grid Cell -> Int -> Int -> Grid Cell
placeBombOnGridLocation grid x y =
  let
    oldCell = Maybe.withDefault defaultCell (Grid.get (x, y) grid)
  in
  Grid.set (x, y) { oldCell | mine = Mined} grid
  

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    OpenCell cell -> 
      ({model | grid = (openCellOnGridLocation model.grid cell.x cell.y)}, Cmd.none)
    FlagCell cell ->
      ({model | grid = flagCellOnGridLocation model.grid cell.x cell.y }, Cmd.none)
    AddBombs bombLocations -> 
      let
        newGrid = Set.foldl (\(x, y) grid -> (placeBombOnGridLocation grid x y)) model.grid bombLocations
        newModel = { model | grid = newGrid }
      in (newModel, Cmd.none)
    HandleRowInputChange input -> 
      let
          newRows = Maybe.withDefault initialRows (String.toInt input)
          newColumns = Grid.height model.grid
          newBombs = 20
          newCommand = initialCommand newRows newColumns newBombs
          newGrid = initialGrid newRows newColumns
          newModel = { model | grid = newGrid}
      in
      (newModel, newCommand)
    HandleColumnInputChange input -> 
      let
          newColumns = Maybe.withDefault initialRows (String.toInt input)
          newRows = Grid.width model.grid
          newBombs = 20
          newCommand = initialCommand newRows newColumns newBombs
          newGrid = initialGrid newRows newColumns
          newModel = { model | grid = newGrid}
      in
      (newModel, newCommand)


-- VIEWS

gridView: Grid Cell -> E.Element Msg
gridView grid =
  E.text "Grid"

controlsView: E.Element Msg
controlsView =
  E.text "Controls"

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
        E.el [E.alignTop, Element.Font.size 50] <| E.text "Minesweeper",
        gridView model.grid,
        E.el [E.alignBottom] controlsView
      ]


-- INITIAL STATE
initialRows = 10
initialColumns = 6
initialBombs = 20

getRandomBombPositions: Int -> Int -> Int -> Random.Generator BombPositions
getRandomBombPositions rows columns bombs = 
  Random.Set.set bombs (Random.pair (Random.int 0 rows) (Random.int 0 columns))

initialGrid: Int -> Int -> Grid Cell
initialGrid rows columns = 
  Grid.repeat rows columns defaultCell

initialCommand rows columns bombs = Random.generate AddBombs (getRandomBombPositions rows columns bombs)

init : () -> (Model, Cmd Msg)
init _ = (Model (initialGrid initialRows initialColumns), initialCommand initialRows initialColumns initialBombs)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}
