module Main exposing (..)
import Browser
import Html exposing (Html, div, text, span)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Set exposing (Set)
import Random
import Random.Set
import Grid exposing (Grid)
import Html.Attributes exposing (style)
import Html exposing (input)
import Html.Attributes exposing (type_, value)
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
    columns = Grid.height grid
    rows = Grid.width grid
    dividor = max rows columns
    gridTemlateRows = String.concat [
      "repeat(",
      String.fromInt(rows),
      ", ",
      String.fromFloat (75 / (toFloat dividor)),
      "vmin)"
      -- "auto)"
      ]
    gridTemplateColumns = String.concat [
      "repeat(",
      String.fromInt(columns),
      ", ",
      String.fromFloat (75 / (toFloat dividor)),
      "vmin)"
      -- "auto)"
      ]
    gridOfDivs =  Grid.indexedMap displayCell grid
    flattenedDivs = Grid.foldl (\cell list -> List.append list [cell]) [] gridOfDivs
  in
  div 
    [ style "display" "grid"
    , style "grid-template-rows" gridTemlateRows -- replace with rows and vmin * 80 / rows
    , style "box-sizing" "border-box"
    , style "grid-template-columns" gridTemplateColumns -- replace with columns and vmin * 80 / columns
    , style "justify-content" "center"
    , style "align-items" "center"
    ] 
    flattenedDivs

header = 
  div 
    [
      style "font-size" "200%",
      style "margin-bottom" "2%"
    ] 
    [text "Minesweeper"]

controls =
  div
    [
      style "display" "flex",
      style "flex-direction" "row",
      style "justify-content" "space-around",
      style "margin-bottom" "1%"
    ]
    [
      span [] [text "Rows: "],
      input [type_ "number", value (String.fromInt initialRows), onInput HandleRowInputChange] [],
      span [] [text "Columns: "],
      input [type_ "number", value (String.fromInt initialColumns), onInput HandleColumnInputChange] []
    ]
view: Model -> Html Msg
view model = 
  div 
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    , style "justify-content" "space-between"
    , style "height" "100vh"
    ]
    [
      header,
      displayGrid model.grid,
      controls
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
