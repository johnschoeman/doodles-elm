module Home exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class)
import Route
import Session exposing (WithSession)


type alias Model =
    WithSession {}


type Msg
    = NoOp


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


view : Model -> Html msg
view model =
    div [ class "flex flex-col ml-4 space-y-4" ]
        [ a [ Route.href Route.Dots ] [ text "Dots" ]
        , a [ Route.href Route.Squares ] [ text "Squares" ]
        , a [ Route.href Route.Recaman ] [ text "Recaman" ]
        , a [ Route.href Route.MothersDay ] [ text "MothersDay" ]
        , a [ Route.href Route.LockPuzzle ] [ text "Lock Puzzle" ]
        ]


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
