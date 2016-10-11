module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Time exposing (Time, second)
import String

type alias Model =
  { tracks : List Track
  , current_beat : Int
  , is_playing : Bool
  , bpm : Int }

type alias Track =
  { beats : List Beat
  , id : Int }

type alias Beat =
  { is_active : Bool
  , id: Int }

initBeat : Int -> Beat
initBeat id =
  { id = id
  , is_active = False }

initTrack : Int -> Track
initTrack id =
  { id = id
  , beats = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] |> List.map initBeat }

initModel : Model
initModel =
  { tracks = [1,2,3,4] |> List.map initTrack
  , bpm = 220
  , is_playing = False
  , current_beat = 1 }

type Msg = UpdateBeat Time | ActivateCell Int Int | Play | Stop

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ActivateCell track_number cell_number ->
          ( model, Cmd.none )
        UpdateBeat time ->
          if model.current_beat == List.length model.tracks then
            ( { model | current_beat = 1 }, Cmd.none )
          else
            ( { model | current_beat = model.current_beat + 1 }, Cmd.none )
        Play ->
          ( { model | is_playing = True  }, Cmd.none )
        Stop ->
          ( { model | is_playing = False }, Cmd.none )

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
  table [ class "table" ]
  [ stepEditorTableHeader
  , stepEditorTracks model ]

stepEditorTableHeader : Html Msg
stepEditorTableHeader =
  tr [] [
    th [] [ text "1" ],
    th [] [ text "2" ],
    th [] [ text "3" ],
    th [] [ text "4" ],
    th [] [ text "5" ],
    th [] [ text "6" ],
    th [] [ text "7" ],
    th [] [ text "8" ],
    th [] [ text "9" ],
    th [] [ text "10" ],
    th [] [ text "11" ],
    th [] [ text "12" ],
    th [] [ text "13" ],
    th [] [ text "14" ],
    th [] [ text "15" ],
    th [] [ text "16" ]
  ]

stepEditorTracks : Model -> Html Msg
stepEditorTracks model =
  model.tracks
  |> List.map stepEditorTrack
  |> tbody []

stepEditorTrack : Track -> Html Msg
stepEditorTrack track =
  track.beats
  |> List.map stepEditorCell
  |> tr []

stepEditorCell : Beat -> Html Msg
stepEditorCell beat  =
  td [ id ( "track-" ++ (toString beat.id) ++ "-cell-" ++ (toString beat.id))
    , class "editor-cell", onClick (ActivateCell beat.id beat.id)
  ] [ text (toString beat.id ++ toString beat.id)]

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
      Time.every (Time.minute * (interval model)) UpdateBeat
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

