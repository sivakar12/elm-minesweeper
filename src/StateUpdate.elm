module StateUpdate exposing (update, addRandomBombsCommand, createEmptyGrid)

import Set
import Grid exposing (Grid)
import Random
import Random.Set

import Types exposing (..)

defaultCell: Cell
defaultCell = Cell Covered NotMined 0

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
  
addNeighborBombsForCell: Grid Cell -> Int -> Int -> Cell
addNeighborBombsForCell grid x y =
  let
    cellOriginal = Maybe.withDefault defaultCell <| Grid.get (x, y) grid
    neighborCellLocation = 
      [
        (x - 1, y - 1),
        (x - 1, y),
        (x - 1, y + 1),
        (x, y - 1),
        (x, y + 1),
        (x + 1, y - 1),
        (x + 1, y),
        (x + 1, y + 1)
      ]
    neighborCells : List (Maybe Cell) 
    neighborCells = List.map (\(i, j) -> Grid.get (i, j) grid) neighborCellLocation
    foldFunction = 
      \maybeCell count -> 
        case maybeCell of
          Maybe.Nothing -> count + 0
          Maybe.Just cell ->
            case cell.mine of
              Mined -> count + 1
              NotMined -> count + 0
    bombCount = List.foldl foldFunction 0 neighborCells
  in
  { cellOriginal | neighboringBombs = bombCount }
countNeighborBombs: Grid Cell -> Grid Cell
countNeighborBombs grid =
  Grid.indexedMap (\x y _ -> addNeighborBombsForCell grid x y ) grid

getRandomBombPositions: Int -> Int -> Int -> Random.Generator BombPositions
getRandomBombPositions width height bombs = 
  Random.Set.set bombs (Random.pair (Random.int 0 width) (Random.int 0 height))

addRandomBombsCommand width height bombs = 
  Random.generate AddBombs (getRandomBombPositions width height bombs)

createEmptyGrid: Int -> Int -> Grid Cell
createEmptyGrid width height = 
  Grid.repeat width height defaultCell

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
        newGridWithBombsAdded = countNeighborBombs newGrid
        newModel = { model | grid = newGridWithBombsAdded }
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
          newCommand = addRandomBombsCommand newWidth newHeight newBombs
          newGrid = createEmptyGrid newWidth newHeight
          newModel = { model | grid = newGrid}
      in
      (newModel, newCommand)
    ToggleFlaggingMode ->
      let
        newModel = { model | flaggingMode =  not model.flaggingMode }
      in
      (newModel, Cmd.none)