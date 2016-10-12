module Beat exposing (Beat, init)

type alias Beat =
  { is_active : Bool
  , id: Int }

init : Int -> Beat
init id =
  { id = id
    , is_active = False }

