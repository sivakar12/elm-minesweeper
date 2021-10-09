module Types exposing (..)

import Grid exposing (Grid)
import Set exposing (Set)

type GameState = 
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

type alias Cell = 
  { covered: CoverState
  , mine: MineState
  , neighboringBombs: Int 
  }

type alias Model = 
  { grid: Grid Cell
  , flaggingMode: Bool
  , gameState: GameState
  }

type ChangeAxis = Width | Height
type ChangeDirection = Increase | Decrease

type alias BombPositions = Set (Int, Int)
type Msg 
  = HandleCellClick{ x: Int, y: Int }
  | AddBombs BombPositions
  | ChangeGridSize ChangeAxis ChangeDirection
  | ToggleFlaggingMode
