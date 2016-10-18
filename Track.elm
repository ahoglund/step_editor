module Track exposing (Track, init)

import Cell exposing (Cell)

type alias Track =
  { id : Int
  , name : String
  , sample_file : String
  , cells : List Cell }

init : Int -> List Cell -> String -> String -> Track
init id cells name sample_file =
  { id = id
  , name = name
  , sample_file = sample_file
  , cells = cells }
