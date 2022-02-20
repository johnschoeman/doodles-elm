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
    div [ class "flex flex-col p-8 space-y-4 text-gray-900" ]
        [ a [ class "lnk text-lg", Route.href Route.ModularTimesTable ] [ text "Modular Times Table" ]
        , a [ class "lnk text-lg", Route.href Route.BlackSheep ] [ text "Black Sheep" ]
        , a [ class "lnk text-lg", Route.href Route.LockPuzzle ] [ text "Lock Puzzle" ]
        , a [ class "lnk text-lg", Route.href Route.Recaman ] [ text "Recaman" ]
        , a [ class "lnk text-lg", Route.href Route.Squares ] [ text "Squares" ]
        , a [ class "lnk text-lg", Route.href Route.Dots ] [ text "Dots" ]
        ]


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
