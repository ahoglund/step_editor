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
  { tracks : List Track
  , total_beats : List Int
  , current_beat : Int
  , is_playing : Bool
  , bpm : Int }

initModel : List Track -> Model
initModel tracks =
  { tracks = tracks
  , total_beats = beatCount
  , bpm = 120
  , is_playing = False
  , current_beat = 1 }

beatCount : List Int
beatCount =
  [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]

trackCount : List Int
trackCount =
  [1,2,3,4,5,6,7,8]

type Msg = SetCurrentBeat Time | ActivateCell Track Beat | Play | Stop

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ActivateCell track beat ->
      let
        tracks = model.tracks
        |> List.map (\t ->
          let
            bts = List.map (\b -> activateBeat t b beat) t.beats
          in
          ({ t | beats = bts })
        )
      in
        ({ model | tracks = tracks }, Cmd.none)
    SetCurrentBeat time ->
      if model.current_beat == List.length model.total_beats then
        ({ model | current_beat = 1 }, Cmd.none )
      else
        ({ model | current_beat = model.current_beat + 1 }, Cmd.none )
    Play ->
      ({ model | is_playing = True  }, Cmd.none )
    Stop ->
      ({ model | is_playing = False }, Cmd.none )

activateBeat : Track -> Beat -> Beat -> Beat
activateBeat track beat1 beat2 =
  if track.id == beat2.track_id && beat1.id == beat2.id  then
    ({ beat1 | is_active = True })
  else
    (beat1)

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
  model.total_beats
  |> List.map (\beat_id -> th [] [ text (toString beat_id) ])
  |> tr []

stepEditorTracks : Model -> Html Msg
stepEditorTracks model =
  model.tracks
  |> List.map (\track -> stepEditorTrack model track)
  |> tbody []

stepEditorTrack : Model -> Track -> Html Msg
stepEditorTrack model track =
  track.beats
  |> List.map (\beat -> stepEditorCell model track beat)
  |> tr []

stepEditorCell : Model -> Track -> Beat -> Html Msg
stepEditorCell model track beat =
  td [ id ("track-" ++ (toString track.id) ++ "-cell-" ++ (toString beat.id))
     , class ((setActiveClass beat.id model.current_beat) ++ " " ++ (setActiveCell track beat))
     , onClick (ActivateCell track beat)] [ ]

setActiveCell : Track -> Beat -> String
setActiveCell track beat =
  if beat.is_active == True && beat.track_id == track.id then
    "success"
  else
    ""

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
    tracks = trackCount
    |> List.map (\track_id ->
        Track.init track_id (
          List.map (\beat_id ->
            Beat.init beat_id track_id
          ) beatCount
        )
       )
    model  = initModel tracks
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

