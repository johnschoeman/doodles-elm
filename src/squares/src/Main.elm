module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrame)
import Color.Convert
import Html exposing (Html, a, div, h1, text)
import Html.Attributes as Attr exposing (class, href, style, type_, value)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, rx, ry, viewBox, width, x, y)
import Task
import Time


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    ()



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "doodles.camp"
    , body =
        [ div [ class "flex flex-col h-full w-full px-8 py-8" ]
            [ header
            , subView model
            ]
        ]
    }


header : Html Msg
header =
    div [ class "flex items-baseline" ]
        [ h1 [ class "text-gray-800 text-3xl lg:text-4xl" ] [ text "Squares" ]
        , a [ class "lnk ml-12", href "https://www.doodles.camp/" ] [ text "back" ]
        ]


type alias XPos =
    Int


type alias YPos =
    Int


type alias XDelta =
    Int


type alias YDelta =
    Int


type alias Color =
    { red : Float
    , green : Float
    , blue : Float
    }


type Square
    = Square Color XPos YPos XDelta YDelta


type alias Model =
    { viewport : Maybe Viewport
    , squares : List Square
    }


type Msg
    = GotViewport Viewport
    | Tick Time.Posix


red : Color
red =
    { red = 100
    , green = 0
    , blue = 0
    }


green : Color
green =
    { red = 0
    , green = 100
    , blue = 0
    }


blue : Color
blue =
    { red = 0
    , green = 0
    , blue = 100
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { viewport = Nothing
      , squares =
            [ Square red 100 200 1 1
            , Square green 200 100 1 2
            , Square blue 50 100 2 3
            , Square red 300 20 2 1
            , Square green 300 50 2 5
            , Square blue 140 20 3 1
            , Square red 90 20 2 2
            , Square green 400 100 0 1
            ]
      }
    , Task.perform GotViewport getViewport
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        Tick now ->
            let
                oldSquares =
                    model.squares
            in
            case model.viewport of
                Just viewport ->
                    ( { model | squares = tickSquares viewport oldSquares }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


tickSquares : Viewport -> List Square -> List Square
tickSquares viewport squares =
    List.map (tickSquare viewport) squares


tickSquare : Viewport -> Square -> Square
tickSquare viewport (Square color xPos yPos xDelta yDelta) =
    let
        { width, height } =
            viewport.viewport

        newXPos =
            modBy (ceiling width) (xPos + xDelta)

        newYPos =
            modBy (ceiling height) (yPos + yDelta)
    in
    Square (nextColor color) newXPos newYPos xDelta yDelta


nextColor : Color -> Color
nextColor color =
    let
        nextValue c rate =
            toFloat <| modBy 255 (ceiling (c + rate))

        nextRed =
            nextValue color.red 0.001

        nextGreen =
            nextValue color.green 0.003

        nextBlue =
            nextValue color.blue 0.002
    in
    { red = nextRed
    , green = nextGreen
    , blue = nextBlue
    }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrame Tick



---- VIEW ----


subView : Model -> Html Msg
subView model =
    case model.viewport of
        Just viewport ->
            div [ class "mt-4" ]
                [ art viewport model
                ]

        Nothing ->
            div [] [ text "loading..." ]


art : Viewport -> Model -> Html Msg
art viewport model =
    let
        { width, height } =
            viewport.viewport

        heightString =
            String.fromInt <| ceiling height

        widthString =
            String.fromInt <| ceiling width
    in
    div [ class "border bg-gray-100" ]
        [ svg [ viewBox <| String.join " " [ "0 0", widthString, heightString ] ]
            (List.map squareSvg model.squares)
        ]


colorToRGB : Color -> String
colorToRGB color =
    "rgba(0,0,0,1.0)"



-- Color.Convert. ( color.red, color.green, color.blue )
-- |> Color.toRGBString


squareSvg : Square -> Html msg
squareSvg (Square color xPos yPos _ _) =
    rect
        [ x <| String.fromInt xPos
        , y <| String.fromInt yPos
        , fill <| colorToRGB color
        , width "100"
        , height "100"
        , rx "2"
        , ry "2"
        ]
        []
