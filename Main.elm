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
      bpm : Int }


initModel : Model
initModel =
    { total_beats   = 16
    , bpm           = 220
    , current_beat  = 1   }

type Msg = UpdateBeat Time | Play | Stop

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateBeat time ->
          if model.current_beat == model.total_beats then
            ( initModel, Cmd.none )
          else
            ( { model | current_beat = model.current_beat + 1 }, Cmd.none )
        Play ->
          ( initModel, Cmd.none )
        Stop ->
          ( initModel, Cmd.none )

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
  table [ class "table table-bordered" ] [ stepEditorTracks model ]

stepEditorTracks : Model -> Html Msg
stepEditorTracks model =
  tr [] [
    td [] [ button [] [ text (toString model.current_beat) ] ]
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
  Time.every (Time.minute * (interval model)) UpdateBeat

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
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

