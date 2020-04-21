module Doodle.Recaman exposing (Model, Msg, init, update, view)

import Browser.Dom exposing (Viewport, getViewport)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Session exposing (WithSession)
import Svg exposing (path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, stroke, strokeWidth, viewBox)
import Task


type alias Model =
    WithSession
        { viewport : Maybe Viewport
        , recaman : Recaman
        , scale : Scale
        }


type alias Recaman =
    List Int


type alias Scale =
    Int


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , viewport = Nothing
      , recaman = [ 0, 1, 2, 3 ]
      , scale = 10
      }
    , Task.perform GotViewport getViewport
    )


type Msg
    = GotViewport Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )


view : Model -> Html msg
view model =
    case model.viewport of
        Just viewport ->
            art viewport model

        Nothing ->
            div [] [ text "loading..." ]


art : Viewport -> Model -> Html msg
art viewport { recaman, scale } =
    let
        { width, height } =
            viewport.viewport

        m =
            min width height

        mString =
            String.fromInt <| ceiling m
    in
    div [ class "border bg-gray-100" ]
        [ svg [ viewBox <| "0 0 " ++ mString ++ " " ++ mString ]
            [ path
                [ pathDrawing
                    [ "M 0 150"
                    , halfCircle 150 50 200 True
                    , halfCircle 150 200 300 False
                    , halfCircle 150 300 350 True
                    ]
                , stroke "black"
                , strokeWidth "1"
                , fill "white"
                ]
                []
            ]
        ]


pathDrawing : List String -> Svg.Attribute msg
pathDrawing parts =
    d <| String.join " " parts


halfCircle : Int -> Int -> Int -> Bool -> String
halfCircle startY startX endX orientation =
    let
        centerX =
            (startX + endX) // 2

        centerY =
            startY

        rx =
            (endX - startX) // 4

        ry =
            rx

        sweepFlag =
            if orientation then
                1

            else
                0
    in
    arc centerX centerY rx ry 0 sweepFlag


arc : Int -> Int -> Int -> Int -> Int -> Int -> String
arc centerX centerY rx ry xRot sweepFlag =
    let
        pieces =
            [ rx, ry, xRot, 0, sweepFlag, centerX, centerY ]

        strings =
            "A" :: List.map String.fromInt pieces
    in
    String.join " " strings
