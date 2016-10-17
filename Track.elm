module Track exposing (Track, init)

import Cell exposing (Cell)

type alias Track =
  { id : Int
  , cells : List Cell }

init : Int -> List Cell -> Track
init id cells =
  { id = id
  , cells = cells }
