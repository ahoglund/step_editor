module Beat exposing (Beat, init)

type alias Beat =
  { is_active : Bool
  , track_id : Int
  , id: Int }

init : Int -> Int -> Beat
init id track_id =
  { id = id
  , track_id = track_id
  , is_active = False }

