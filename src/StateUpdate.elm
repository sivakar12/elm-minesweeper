module StateUpdate exposing (update, addRandomBombsCommand, createEmptyGrid, getBombCountFromGridSize)

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

toggleFlagOnGridLocation: Grid Cell -> Int -> Int -> Grid Cell
toggleFlagOnGridLocation grid x y =
  let
    oldCell = Maybe.withDefault defaultCell (Grid.get (x, y) grid)
    newCoveredState =
      case oldCell.covered of
        Flagged -> Covered
        Covered -> Flagged
        Opened -> Opened
  in
  Grid.set (x, y) { oldCell | covered = newCoveredState } grid

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

-- a game is won when all the cells that are not mined are uncovered
isGameWon: Grid Cell -> Bool
isGameWon grid =
  let
    foldFunction: Cell -> Bool -> Bool
    foldFunction cell goodSoFar =
      let
        wonState = -- are these combinations what you would find in a finished game
          case (cell.mine, cell.covered) of
            (Mined, Covered) -> True
            (Mined, Opened) -> False
            (Mined, Flagged) -> True
            (NotMined, Covered) -> False
            (NotMined, Opened) -> True
            (NotMined, Flagged) -> False
      in
        goodSoFar && wonState

  in
  Grid.foldl foldFunction True grid

isExplosion: Grid Cell -> Int -> Int -> Bool
isExplosion grid x y =
  let
    cell = Maybe.withDefault defaultCell <| Grid.get (x, y) grid
  in  
    case (cell.mine) of
      Mined -> True
      _ -> False

getBombCountFromGridSize: Int -> Int -> Int
getBombCountFromGridSize width height =
  round <| 0.2 * (toFloat width) * (toFloat height)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    HandleCellClick cell ->
      case model.gameState of
        Playing ->
          let
            newGrid =
              case model.flaggingMode of
                False ->
                  openCellOnGridLocation model.grid cell.x cell.y
                True ->
                  toggleFlagOnGridLocation model.grid cell.x cell.y
            isGameLost = (isExplosion model.grid cell.x cell.y) && (not model.flaggingMode)
            newGameState = if (isGameWon newGrid) then Finished True else (if isGameLost then Finished False else Playing)
          in
          ({model | grid = newGrid, gameState = newGameState }, Cmd.none)
        _ -> (model, Cmd.none) -- do nothing when the state is not playing
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
          newBombs = getBombCountFromGridSize newWidth newHeight
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
    StartGame ->
      ({model | gameState = Playing }, Cmd.none)