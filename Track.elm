module Track exposing (Track, init)

import Beat exposing (Beat)

type alias Track =
  { id : Int
  , beats : List Beat }

init : Int -> List Beat -> Track
init id beats =
  { id = id
  , beats = beats }
