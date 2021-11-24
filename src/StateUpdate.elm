module StateUpdate exposing 
  ( update  
  , addRandomBombsCommand
  , createEmptyGrid
  , getBombCountFromGridSize
  , getRandomBombPositions
  )

import Set
import Grid exposing (Grid)
import Random
import Random.Set
import Debug exposing (toString)

import Types exposing (..)

defaultCell: Cell
defaultCell = 
  Cell Covered NotMined 0


createEmptyGrid: Int -> Int -> Grid Cell
createEmptyGrid width height = 
  Grid.repeat width height defaultCell


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
    cellOriginal = 
      Grid.get (x, y) grid
      |>  Maybe.withDefault defaultCell  

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
    neighborCells = 
      List.map 
        (\(i, j) -> Grid.get (i, j) grid) 
        neighborCellLocation

    foldBombCounts = 
      \maybeCell count -> 
        case maybeCell of
          Maybe.Nothing -> count + 0
          Maybe.Just cell ->
            case cell.mine of
              Mined -> count + 1
              NotMined -> count + 0
    
    bombCount = List.foldl foldBombCounts 0 neighborCells
  in
  { cellOriginal | neighboringBombs = bombCount }


countNeighborBombs: Grid Cell -> Grid Cell
countNeighborBombs grid =
  Grid.indexedMap 
    (\x y _ -> addNeighborBombsForCell grid x y ) 
    grid


getRandomBombPositions: Int -> Int -> Int -> Random.Generator BombPositions
getRandomBombPositions width height bombs = 
  Random.Set.set 
    bombs 
    (Random.pair (Random.int 0 (height - 1)) (Random.int 0 (width - 1)))


addRandomBombsCommand : Int -> Int -> Int -> Cmd Msg
addRandomBombsCommand width height bombs = 
  Random.generate 
    AddBombs 
    (getRandomBombPositions width height bombs)


-- a game is won when all the cells that are not mined are uncovered
isGameWon: Grid Cell -> Bool
isGameWon grid =
  let
    foldFinishCombinations: Cell -> Bool -> Bool
    foldFinishCombinations cell goodSoFar =
      let
        wonState = 
        -- are these combinations what you would find in a finished game
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
  Grid.foldl foldFinishCombinations True grid


isExplosion: Grid Cell -> Int -> Int -> Bool
isExplosion grid x y =
  let
    cell =
      Grid.get (x, y) grid 
      |> Maybe.withDefault defaultCell
  in  
    case cell.mine of
      Mined -> True
      _ -> False


getBombCountFromGridSize: Int -> Int -> Int
getBombCountFromGridSize width height =
  round <| 0.2 * (toFloat width) * (toFloat height)

neighborCellLocations : (Int, Int) -> List (Int, Int)
neighborCellLocations (x, y)= 
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
openAdjacentEmptyCells: Grid Cell -> Set.Set (Int, Int) -> Grid Cell
openAdjacentEmptyCells grid thingsToRemove =
  -- this is a recursive function
  -- if the set is empty return the grid
  -- first uncover the cells in the cell if it is zero (covered is changed from true to false)
  -- then find the neighbors of these cells
  -- filter these neighbors to find only those that need clearning (mine count zero, covered true)
  -- pass this new set to the next function
  if Set.isEmpty thingsToRemove
    then grid
  else
    let
      newGrid =
        Set.foldl
          (\(x, y) g -> openCellOnGridLocation g x y)
          grid
          thingsToRemove
      thingsToRemoveWithZeros = 
        Set.filter 
          (
            \(x, y) -> 
              case (Grid.get (x, y) newGrid) of
                 Maybe.Just cell -> if (cell.neighboringBombs == 0 && cell.mine == NotMined) then True else False
                 Maybe.Nothing -> False
          )
          thingsToRemove
      allNeigibhorsWithDuplicates =
       Set.map neighborCellLocations thingsToRemoveWithZeros
      neighborsWithoutDuplicates = 
        Set.foldl
          (\pointsList uniquePoints -> Set.union uniquePoints (Set.fromList pointsList))
          Set.empty
          allNeigibhorsWithDuplicates
      newThingsToRemove =
        Set.filter
          (
            \(x, y) -> 
              case (Grid.get (x, y) newGrid) of
                Maybe.Just cell -> if (cell.covered == Covered && cell.mine == NotMined) then True else False
                Maybe.Nothing -> False
          )
          neighborsWithoutDuplicates
    in
    openAdjacentEmptyCells newGrid newThingsToRemove 
  

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    HandleCellClick cell ->
      case model.gameState of
        Playing ->
          let
            newGrid =
              if model.flaggingMode then
                  toggleFlagOnGridLocation model.grid cell.x cell.y
              else
                  openCellOnGridLocation model.grid cell.x cell.y
                  |> (\grid -> openAdjacentEmptyCells grid (Set.fromList [(cell.x, cell.y)]))
            
            isGameLost = 
              (isExplosion model.grid cell.x cell.y) 
              && (not model.flaggingMode)
            
            newGameState = 
              if (isGameWon newGrid) then 
                  Finished True 
                else if isGameLost then 
                  Finished False 
                else 
                  Playing
          in
          (
            { model 
            | grid = newGrid
            , gameState = newGameState
            , flaggingMode = False
            },
            Cmd.none
          )
        -- do nothing when the state is not playing
        _ -> (model, Cmd.none) 

    AddBombs bombLocations -> 
      let
        _ = Debug.log "bombs generated: " <| toString <| bombLocations
        newGrid = 
          Set.foldl 
            (\(x, y) grid -> (placeBombOnGridLocation grid x y)) 
            model.grid
            bombLocations
        
        newGridWithBombsAdded =
          countNeighborBombs newGrid

        newModel = 
          { model | grid = newGridWithBombsAdded }
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
          newGrid = createEmptyGrid newWidth newHeight
          newModel = 
            { model | grid = newGrid}
      in
      (newModel, Cmd.none)

    ToggleFlaggingMode ->
      let
        newModel = { model | flaggingMode =  not model.flaggingMode }
      in
      (newModel, Cmd.none)

    StartGame ->
      let
        height = Grid.height model.grid
        width = Grid.width model.grid
        bombs = getBombCountFromGridSize width height
        newGrid = createEmptyGrid width height
        newCommand = addRandomBombsCommand height width bombs
      in
      ({ model | grid = newGrid, gameState = Playing }, newCommand)