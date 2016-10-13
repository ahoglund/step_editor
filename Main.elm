module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Time exposing (Time, second)
import String
import Track exposing (Track)
import Beat exposing (Beat)

type alias Model =
  { beats : List Beat
  , tracks : List Track
  , current_beat : Int
  , is_playing : Bool
  , bpm : Int }

initModel : Model
initModel =
  { beats  = (List.map Beat.init beatCount)
  , tracks = (List.map Track.init trackCount)
  , bpm = 120
  , is_playing = False
  , current_beat = 1 }

beatCount : List Int
beatCount =
  [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]

trackCount : List Int
trackCount =
  [1,2,3,4]

type Msg = SetCurrentBeat Time | ActivateCell Int Int | Play | Stop

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ActivateCell track_number cell_number ->
      let
        beats = model.beats
        |> List.map (\beat -> activateBeat track_number cell_number beat)
      in
        ({ model | beats = beats }, Cmd.none)
    SetCurrentBeat time ->
      if model.current_beat == List.length model.beats then
        ( { model | current_beat = 1 }, Cmd.none )
      else
        ( { model | current_beat = model.current_beat + 1 }, Cmd.none )
    Play ->
      ( { model | is_playing = True  }, Cmd.none )
    Stop ->
      ( { model | is_playing = False }, Cmd.none )

activateBeat : Int -> Int -> Beat -> Beat
activateBeat track_number cell_number beat =
  if beat.id == cell_number then
    { beat | is_active = True }
  else
    beat

stepEditorSection : Model -> Html Msg
stepEditorSection model =
  div [ class "page-header" ]
    [ stepEditorHeader,
      stepEditor model ]

stepEditorHeader : Html Msg
stepEditorHeader =
  h3 [] [ text ("Drum Sequence Editor") ]

stepEditor : Model -> Html Msg
stepEditor model =
  table [ class "table table-hover table-bordered" ]
  [ stepEditorTableHeader model
  , stepEditorTracks model ]

stepEditorTableHeader : Model -> Html Msg
stepEditorTableHeader model =
  model.beats
  |> List.map (\track -> th [] [ text (toString track.id) ])
  |> tr []

stepEditorTracks : Model -> Html Msg
stepEditorTracks model =
  model.tracks
  |> List.map (\track -> stepEditorTrack model track)
  |> tbody []

stepEditorTrack : Model -> Track -> Html Msg
stepEditorTrack model track =
  model.beats
  |> List.map (\beat -> stepEditorCell model track beat)
  |> tr []

stepEditorCell : Model -> Track -> Beat -> Html Msg
stepEditorCell model track beat =
  td [ id ("track-" ++ (toString track.id) ++ "-cell-" ++ (toString beat.id))
     , class ((setActiveClass beat.id model.current_beat) ++ " " ++ setActiveCell beat)
     , onClick (ActivateCell track.id beat.id)] [ ]

setActiveCell : Beat -> String
setActiveCell beat =
  if beat.is_active == True then
    "success"
  else
    "off"

setActiveClass : Int -> Int -> String
setActiveClass beat_id current_beat =
  if beat_id == current_beat then
    "active"
  else
    "inactive"

buttons : Html Msg
buttons =
  div []
  [
    button [ class "btn btn-default" , onClick Play ] [ text "Play" ],
    button [ class "btn btn-default" , onClick Stop ] [ text "Stop" ]
  ]

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ stepEditorSection model,
          buttons ]

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.is_playing of
    True ->
      Time.every (Time.minute * (interval model)) SetCurrentBeat
    False ->
      Sub.none

interval : Model -> Float
interval model =
    1 / (toFloat model.bpm)

init =
  let
    model = initModel
  in
    (model, Cmd.none)

main : Program Never
main =
  App.program
    { init          = init
    , subscriptions = subscriptions
    , update        = update
    , view          = view
    }

