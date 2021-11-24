module StateUpdateTests exposing (suite)

import Expect
import Test exposing (..)


import Grid
import Types exposing (..)
import StateUpdate exposing 
  ( createEmptyGrid
  , getBombCountFromGridSize
  -- , getRandomBombPositions
  )
import Maybe

suite: Test
suite =
  describe "The StateUpdate module" 
    [ describe "createEmptyGrid function"
      [ test "returns a grid with the exact size" <|
          \_ ->
            let
              defaultCell = Cell Covered NotMined 0
              expected = 
                Grid.fromList 
                  [ [ defaultCell, defaultCell, defaultCell ]
                  , [ defaultCell, defaultCell, defaultCell ]
                  , [ defaultCell, defaultCell, defaultCell ]
                  ]
              returned = createEmptyGrid 3 3
            in
              Expect.equal expected (Maybe.Just returned)
      ],
      describe "getBombCountFromGridSize function"
      [
        test "returns the correct number of bombs" <|
          \_ ->
            let
              returned = getBombCountFromGridSize 11 13
            in
              Expect.equal 29 returned
      ]
      -- describe "getRandomBombPositions function"
      -- [
      --   test "returns the correct number of positions" <|
      --     \_ ->
      --       let
      --         generator = getRandomBombPositions 10 10 10

      --       in
      --         Expect.equal 10 returned.length
      -- ]
      -- ] 
    ]