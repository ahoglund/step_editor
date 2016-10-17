module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Time exposing (Time, second)
import String
import Track exposing (Track)
import Cell exposing (Cell)

type alias Model =
  { tracks : List Track
  , total_beats : List Int
  , current_beat : Maybe Int
  , is_playing : Bool
  , bpm : Int }

initModel : List Track -> Model
initModel tracks =
  { tracks = tracks
  , total_beats = beatCount
  , bpm = 250
  , is_playing = False
  , current_beat = Nothing }

beatCount : List Int
beatCount =
  [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]

trackCount : List Int
trackCount =
  [1,2,3,4,5,6,7,8]

type Msg = SetCurrentBeat Time | ToggleCell Track Cell | Play | Stop

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleCell track beat ->
      let
        tracks = model.tracks
        |> List.map (\t ->
          let
            new_beats = List.map (\b -> activateCell t b beat) t.beats
          in
          ({ t | beats = new_beats })
        )
      in
        ({ model | tracks = tracks }, Cmd.none)
    SetCurrentBeat time ->
      case model.current_beat of
        Nothing ->
          if model.is_playing == True then
            ({ model | current_beat = Just 1 }, Cmd.none )
          else
            (model, Cmd.none)
        Just beat ->
          if beat == List.length model.total_beats then
            ({ model | current_beat = Just 1 }, Cmd.none )
          else
            ({ model | current_beat = Just (beat + 1)}, Cmd.none )
    Play ->
      if model.is_playing == True then
        ({ model | current_beat = Just 1 }, Cmd.none )
      else
        ({ model | is_playing = True }, Cmd.none )
    Stop ->
      if model.is_playing == False then
        ({ model | current_beat = Nothing, is_playing = False }, Cmd.none )
      else
        ({ model | is_playing = False }, Cmd.none )

activateCell : Track -> Cell -> Cell -> Cell
activateCell track beat1 beat2 =
  if track.id == beat2.track_id && beat1.id == beat2.id  then
    if beat1.is_active == True then
      ({ beat1 | is_active = False })
    else
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

stepEditorCell : Model -> Track -> Cell -> Html Msg
stepEditorCell model track beat =
  td [ id ("track-" ++ (toString track.id) ++ "-cell-" ++ (toString beat.id))
     , class ((setActiveClass beat.id model.current_beat) ++ " " ++ (setActiveCell track beat))
     , onClick (ToggleCell track beat)] [ ]

setActiveCell : Track -> Cell -> String
setActiveCell track beat =
  if beat.is_active == True && beat.track_id == track.id then
    "success"
  else
    ""

setActiveClass : Int -> Maybe Int -> String
setActiveClass beat_id current_beat =
  case current_beat of
    Nothing ->
      "inactive"
    Just beat ->
      if beat_id == beat then
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
            Cell.init beat_id track_id
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

