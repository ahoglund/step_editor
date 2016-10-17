module Track exposing (Track, init)

import Cell exposing (Cell)

type alias Track =
  { id : Int
  , beats : List Cell }

init : Int -> List Cell -> Track
init id beats =
  { id = id
  , beats = beats }
