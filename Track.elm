module Track exposing (Track, init)

type alias Track =
  { id : Int }

init : Int -> Track
init id =
  { id = id }
