module Doodle.BlackSheep exposing (Model, Msg, init, update, view)

import Browser.Dom exposing (Viewport, getViewport)
import Html exposing (Html, div, text)
import Session exposing (WithSession)
import Task


type alias Model =
    WithSession
        { viewport : Maybe Viewport
        }


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , viewport = Nothing
      }
    , Cmd.batch [ Task.perform GotViewport getViewport ]
    )


type Msg
    = GotViewport Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.viewport of
        Just viewport ->
            div [] [ text "BlackSheep" ]

        Nothing ->
            div [] [ text "loading..." ]
