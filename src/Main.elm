module Main exposing (..)
import Browser
import Set exposing (Set)
import Html exposing (Html)
import Random
import Random.Set
import Grid exposing (Grid)
import Element as E
import Element.Font
import Element.Input
import Element.Border
import Maybe
import Array

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

type ChangeAxis = Width | Height
type ChangeDirection = Increase | Decrease

type alias BombPositions = Set (Int, Int)
type Msg 
  = OpenCell { x: Int, y: Int }
  | FlagCell { x: Int, y: Int }
  | AddBombs BombPositions
  | ChangeGridSize ChangeAxis ChangeDirection


-- STATE UPDATE HANDLERS
defaultCell: Cell
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
    ChangeGridSize axis direction ->
      let
          change = 
            case direction of
              Increase -> 1
              Decrease -> -1
          oldWidth = Grid.width model.grid
          oldHeight = Grid.height model.grid
          newWidth = 
            case axis of 
              Width -> oldWidth + change
              Height -> oldWidth
          newHeight = 
            case axis of
              Width -> oldHeight
              Height -> oldHeight + change
          newBombs = 10 -- Todo: Change to 0.2 * new rows * new columns
          newCommand = initialCommand newWidth newHeight newBombs
          newGrid = initialGrid newWidth newHeight
          newModel = { model | grid = newGrid}
      in
      (newModel, newCommand)

-- there are as many rows as height in grid
-- VIEWS

displayCell: Int -> Int -> Cell -> E.Element Msg
displayCell x y cell =
  E.text "O"

displayRow: Int -> Array.Array Cell -> E.Element Msg
displayRow x row =
  E.row
    []
    <| Array.toList <| Array.indexedMap (\y cell -> (displayCell x y cell)) <| row
gridView: Grid Cell -> E.Element Msg
gridView grid =
  E.column
    []
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
      Element.Input.button controlButtonStyles { label = E.text "➖", onPress = Maybe.Just <| ChangeGridSize Height Decrease } 
    ]

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
        E.el [E.alignBottom, E.centerX] controlsView
      ]


-- INITIAL STATE
initialWidth = 10
initialHeight = 6
initialBombs = 20

getRandomBombPositions: Int -> Int -> Int -> Random.Generator BombPositions
getRandomBombPositions width height bombs = 
  Random.Set.set bombs (Random.pair (Random.int 0 height) (Random.int 0 width))

initialGrid: Int -> Int -> Grid Cell
initialGrid width height = 
  Grid.repeat width height defaultCell

initialCommand width height bombs = Random.generate AddBombs (getRandomBombPositions width height bombs)

init : () -> (Model, Cmd Msg)
init _ = (Model (initialGrid initialWidth initialHeight), initialCommand initialWidth initialHeight initialBombs)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}
