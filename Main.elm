module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Time exposing (Time, second)
import String

type alias Model =
    { total_beats : Int,
      current_beat : Int,
      playing : Bool,
      bpm : Int }


initModel : Model
initModel =
    { total_beats   = 16
    , bpm           = 220
    , playing       = False
    , current_beat  = 1   }

type Msg = UpdateBeat Time | Play | Stop

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateBeat time ->
          if model.current_beat == model.total_beats then
            ( { model | current_beat = 1 }, Cmd.none )
          else
            ( { model | current_beat = model.current_beat + 1 }, Cmd.none )
        Play ->
          ( { model | playing = True  }, Cmd.none )
        Stop ->
          ( { model | playing = False }, Cmd.none )

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
  [
    stepEditorTableHeader
  , stepEditorTracks model 1
  , stepEditorTracks model 2
  , stepEditorTracks model 3
  , stepEditorTracks model 4
  ]

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

stepEditorTracks : Model -> Int -> Html Msg
stepEditorTracks model track_number =
  tr [] [
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-1"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-2"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-3"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-4"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-5"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-6"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-7"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-8"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-9"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-10"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-11"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-12"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-13"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-14"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-15"), class "editor-cell"] [ ],
    td [ id ( "track-" ++ (toString track_number) ++ "-cell-16"), class "editor-cell"] [ ]
  ]

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
  case model.playing of
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

