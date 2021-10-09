module Types exposing (..)

import Grid exposing (Grid)
import Set exposing (Set)

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

type alias Cell = { covered: CoverState, mine: MineState, neighboringBombs: Int }
type alias Model = { grid: Grid Cell }

type ChangeAxis = Width | Height
type ChangeDirection = Increase | Decrease

type alias BombPositions = Set (Int, Int)
type Msg 
  = OpenCell { x: Int, y: Int }
  | FlagCell { x: Int, y: Int }
  | AddBombs BombPositions
  | ChangeGridSize ChangeAxis ChangeDirection
