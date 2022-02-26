module Home exposing (Model, Msg, init, subscriptions, update, view)

import Doodle.OrthoBoard.Main as OrthoBoard
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class)
import Route
import Session exposing (WithSession)


type alias Model =
    WithSession { subModel : OrthoBoard.Model }


type Msg
    = HandleOrthoBoardMsg OrthoBoard.Msg
    | NoOp


init : Session.Model -> ( Model, Cmd Msg )
init session =
    let
        ( initModel, initMsg ) =
            OrthoBoard.init
    in
    ( { session = session, subModel = initModel }, Cmd.map HandleOrthoBoardMsg initMsg )


view : Model -> Html Msg
view model =
    div [ class "lg:h-[80vh] lg:flex lg:flex-row justify-between" ]
        [ div [ class "lg:w-96 flex flex-col p-8 space-y-4 text-gray-900" ]
            [ a [ class "lnk text-lg", Route.href Route.ModularTimesTable ] [ text "modular times table" ]
            , a [ class "lnk text-lg", Route.href Route.BlackSheep ] [ text "black sheep" ]
            , a [ class "lnk text-lg", Route.href Route.LockPuzzle ] [ text "lock puzzle" ]
            , a [ class "lnk text-lg", Route.href Route.Recaman ] [ text "recaman" ]
            , a [ class "lnk text-lg", Route.href Route.Squares ] [ text "squares" ]
            , a [ class "lnk text-lg", Route.href Route.Dots ] [ text "dots" ]
            ]
        , div [ class "scroll-y-auto w-full p-12 flex justify-end" ]
            [ pageContent model
            ]
        ]


pageContent : Model -> Html Msg
pageContent { subModel } =
    Html.map HandleOrthoBoardMsg <| OrthoBoard.view subModel


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( HandleOrthoBoardMsg subMsg, { subModel } ) ->
            let
                ( newModel, newMsg ) =
                    OrthoBoard.update subMsg subModel
            in
            ( { model | subModel = newModel }, Cmd.none )

        ( NoOp, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
