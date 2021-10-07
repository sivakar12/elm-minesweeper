module Main exposing (..)
import Browser
import Html exposing (Html, div, text, span)
import Html.Events exposing (onClick, onDoubleClick)
import Set exposing (Set)
import Random
import Random.Set
import Grid exposing (Grid)
import Html.Attributes exposing (style)

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
    AddBombs bombs -> 
      let
        newGrid = Set.foldl (\(x, y) grid -> (placeBombOnGridLocation grid x y)) model.grid bombs
        newModel = { model | grid = newGrid }
      in (newModel, Cmd.none)


-- VIEWS

cellStyles = 
  [ style "aspect-ratio" "1"
  , style "border" "2px solid black"
  , style "cursor" "pointer"
  , style "display" "flex"
  , style "justify-content" "center"
  , style "align-items" "center"
  , style "user-select" "none"
  ]

displayCell: Int -> Int -> Cell -> Html Msg
displayCell x y { covered, mine } =
  let
    clickHandlers = 
      [
        onClick (OpenCell{x = x, y = y}), 
        onDoubleClick (FlagCell { x = x, y = y})
      ]
  in
  case (covered, mine) of
    (Covered, _) -> span (cellStyles ++ clickHandlers) [text "📦"]
    (Opened, Mined) -> span cellStyles [text "💣"]
    (Opened, NotMined) -> span cellStyles [text "8"] -- calculate and put actual number
    (Flagged, _) -> span cellStyles [text "🚩"]


displayGrid: Grid Cell -> Html Msg
displayGrid grid = 
  let
    gridOfDivs =  Grid.indexedMap displayCell grid
    flattenedDivs = Grid.foldl (\cell list -> List.append list [cell]) [] gridOfDivs
  in
  div 
    [ style "display" "grid"
    , style "grid-template-rows" "repeat(10, auto)"
    , style "grid-template-columns" "repeat(10, auto)"
    , style "justify-content" "center"
    , style "align-content" "center"
    ] 
    flattenedDivs

header = 
  div 
    [
      style "font-size" "200%"
    ] 
    [text "Minesweeper"]

view: Model -> Html Msg
view model = 
  div 
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    , style "justify-content" "center"
    , style "height" "100vh"
    ]
    [
      header,
      displayGrid model.grid
    ]


-- INITIAL STATE

getRandomBombPositions: Int -> Int -> Int -> Random.Generator BombPositions
getRandomBombPositions count rows columns = 
  Random.Set.set count (Random.pair (Random.int 0 rows) (Random.int 0 columns))

initialGrid: Int -> Int -> Grid Cell
initialGrid rows columns = 
  Grid.repeat rows columns defaultCell

initialCommand = Random.generate AddBombs (getRandomBombPositions 10 10 10)

init : () -> (Model, Cmd Msg)
init _ = (Model (initialGrid 10 10), initialCommand)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}
