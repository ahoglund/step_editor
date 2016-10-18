module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Time exposing (Time, second)
import String
import Track exposing (Track)
import Cell exposing (Cell)
import Cmds exposing (..)

type alias Model =
  { tracks : List Track
  , total_beats : Int
  , current_beat : Maybe Int
  , is_playing : Bool
  , bpm : Int }

initModel : List Track -> Model
initModel tracks =
  { tracks = tracks
  , total_beats = List.length beatCount
  , bpm = 180
  , is_playing = False
  , current_beat = Nothing }

beatCount : List Int
beatCount =
  [1..16]

trackCount : List { id : number, name : String, sample_file : String }
trackCount =
  [
    { id = 1, name = "Kick", sample_file = "samples/kick.wav" },
    { id = 2, name = "Snare", sample_file = "samples/snare.wav" },
    { id = 3, name = "HH Closed", sample_file = "samples/hh-closed.wav" },
    { id = 4, name = "HH Open", sample_file = "samples/hh-open.wav" }
  ]

type Msg = SetCurrentBeat Time
  | PlaySound String
  | ToggleCell Track Cell
  | Play
  | Stop

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    PlaySound file ->
      (model, Cmds.playSound(file))
    ToggleCell track beat ->
      let
        tracks = model.tracks
        |> List.map (\t ->
          let
            new_cells = List.map (\b -> toggleCell t b beat) t.cells
          in
          ({ t | cells = new_cells })
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
          if beat == model.total_beats then
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

toggleCell : Track -> Cell -> Cell -> Cell
toggleCell track cell1 cell2 =
  if track.id == cell2.track_id && cell1.id == cell2.id  then
    if cell1.is_active == True then
      ({ cell1 | is_active = False })
    else
      ({ cell1 | is_active = True })
  else
    (cell1)

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
  [1..model.total_beats]
  |> List.map (\beat_id -> th [] [ text (toString beat_id) ])
  |> List.append [th [] [ text "Sample" ]]
  |> tr []

stepEditorTracks : Model -> Html Msg
stepEditorTracks model =
  model.tracks
  |> List.map (\track -> stepEditorTrack model track)
  |> tbody []

stepEditorTrack : Model -> Track -> Html Msg
stepEditorTrack model track =
  let
  preview_cell =
    td []
      [ button [ class "btn btn-default" , onClick (PlaySound track.sample_file)]
        [ text track.name ]
      ]
  in
    track.cells
    |> List.map (\beat -> stepEditorCell model track beat)
    |> List.append [preview_cell]
    |> tr []

stepEditorCell : Model -> Track -> Cell -> Html Msg
stepEditorCell model track beat =
  td [ id ("track-" ++ (toString track.id) ++ "-cell-" ++ (toString beat.id))
     , class ((setActiveClass beat.id model.current_beat) ++ " " ++ (setActiveCell track beat))
     , onClick (ToggleCell track beat)] []

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

buttons : Model -> Html Msg
buttons model =
  div []
  [
    button [ class "btn btn-success" , onClick Play ]
      [ span [ class "glyphicon glyphicon-play" ] [] ],
    button [ class "btn btn-danger" , onClick Stop ]
      [ span [ class "glyphicon glyphicon-stop" ] [] ]
  ]

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ stepEditorSection model,
          buttons model ]

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
    |> List.map (\track_details ->
        Track.init
          track_details.id
          (List.map (\cell_id -> Cell.init cell_id track_details.id) beatCount)
          track_details.name
          track_details.sample_file
       )
    model = initModel tracks
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

