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
        [ a [ class "lnk", Route.href Route.Dots ] [ text "Dots" ]
        , a [ class "lnk", Route.href Route.Squares ] [ text "Squares" ]
        , a [ class "lnk", Route.href Route.Recaman ] [ text "Recaman" ]
        , a [ class "lnk", Route.href Route.MothersDay ] [ text "Mothers Day" ]
        , a [ class "lnk", Route.href Route.LockPuzzle ] [ text "Lock Puzzle" ]
        , a [ class "lnk", Route.href Route.BlackSheep ] [ text "Black Sheep" ]
        ]


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
