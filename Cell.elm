module Cell exposing (Cell, init, initCells)

type alias Cell =
  { is_active : Bool
  , track_id : Int
  , id: Int }

init : Int -> Int -> Cell
init id track_id =
  { id = id
  , track_id = track_id
  , is_active = False }

initCells : List Int
initCells =
  [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
