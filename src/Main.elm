module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Color
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, viewBox, width, x, y)
import Svg.Events
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { viewport : Maybe Viewport
    , colNumber : Int
    , baseColor : DotColor
    }


type PrimaryColor
    = Red
    | Green
    | Blue


type alias DotColor =
    { red : Int
    , green : Int
    , blue : Int
    }


showDotColor : DotColor -> String
showDotColor { red, green, blue } =
    "Red: "
        ++ String.fromInt red
        ++ " Green: "
        ++ String.fromInt green
        ++ " Blue: "
        ++ String.fromInt blue


type Msg
    = GotViewport Viewport
    | IncrementColNumber
    | DecrementColNumber
    | ClickDot DotColor
    | RandomizeColor Int
    | GetRandomColor


init : () -> ( Model, Cmd Msg )
init flags =
    ( { viewport = Nothing
      , colNumber = 6
      , baseColor = { red = 12, green = 12, blue = 12 }
      }
    , Task.perform GotViewport getViewport
    )


generateBaseColor : Cmd Msg
generateBaseColor =
    Random.generate RandomizeColor <| Random.int 1 12


view : Model -> Html Msg
view model =
    let
        buttonStyle =
            "mr-4 bg-transparent hover:bg-blue-500 text-blue-700 font-semibold hover:text-white py-2 px-4 border border-blue-500 hover:border-transparent rounded"
    in
    case model.viewport of
        Just viewport ->
            div [ class "p-32" ]
                [ div []
                    [ button [ class buttonStyle, onClick IncrementColNumber ] [ text "+" ]
                    , button [ class buttonStyle, onClick DecrementColNumber ] [ text "-" ]
                    , button [ class buttonStyle, onClick GetRandomColor ] [ text "R" ]
                    ]
                , div [] [ text <| showDotColor model.baseColor ]
                , art viewport model.colNumber model.baseColor
                ]

        Nothing ->
            div [] [ text "loading..." ]


art : Viewport -> Int -> DotColor -> Html Msg
art viewport colNumber baseColor =
    let
        { width, height } =
            viewport.viewport

        m =
            min width height

        w =
            ceiling <| (m + 100) / toFloat (colNumber * 2)
    in
    div [ class "grid my-vmin m-auto border-2" ] (listOfDots baseColor w colNumber)


listOfDots : DotColor -> Int -> Int -> List (Html Msg)
listOfDots baseColor w count =
    let
        baseMatrix =
            List.repeat count (List.repeat count 0)
    in
    List.indexedMap (\rowIdx row -> rowToDots baseColor w rowIdx row) baseMatrix


rowToDots : DotColor -> Int -> Int -> List a -> Html Msg
rowToDots baseColor w rowIdx row =
    div [ class "flex flex-row" ]
        (List.indexedMap (\colIdx _ -> dot baseColor w rowIdx colIdx) row)


dot : DotColor -> Int -> Int -> Int -> Html Msg
dot { red, green, blue } w rowIdx colIdx =
    let
        nextRed =
            toFloat (modBy 255 (red * colIdx))

        nextGreen =
            toFloat (modBy 255 (green * (rowIdx + colIdx)))

        nextBlue =
            toFloat (modBy 255 (blue * rowIdx))

        dotColor =
            DotColor red green blue

        fillColor =
            Color.fromRGB ( nextRed, nextGreen, nextBlue )
                |> Color.toRGBString

        wString =
            String.fromInt w

        rString =
            String.fromInt (w // 2)
    in
    div [ class "w-full h-full flex justify-center items-center" ]
        [ svg [ Svg.Events.onClick <| ClickDot dotColor, width wString, height wString, viewBox ("0 0 " ++ wString ++ " " ++ wString) ]
            [ circle
                [ cx rString
                , cy rString
                , fill fillColor
                , r rString
                , width <| String.fromInt w
                , height <| String.fromInt w
                ]
                []
            ]
        ]


square : Html msg
square =
    svg
        [ width "400"
        , height "400"
        , viewBox "0 0 400 400"
        ]
        [ rect
            [ x "10"
            , y "10"
            , fill "blue"
            , width "100"
            , height "100"
            , rx "2"
            , ry "2"
            ]
            []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        IncrementColNumber ->
            ( { model | colNumber = model.colNumber + 1 }, Cmd.none )

        DecrementColNumber ->
            ( { model | colNumber = model.colNumber - 1 }, Cmd.none )

        ClickDot oldColor ->
            let
                nextColor =
                    getNextColor oldColor
            in
            ( { model | baseColor = nextColor }, Cmd.none )

        RandomizeColor color ->
            let
                nextColor =
                    DotColor color 12 12
            in
            ( { model | baseColor = nextColor }, Cmd.none )

        GetRandomColor ->
            ( model, generateBaseColor )


getNextColor : DotColor -> DotColor
getNextColor color =
    let
        maxColor =
            if Debug.log "red" color.red > color.green && color.green > color.blue then
                Red

            else if Debug.log "green" color.green > color.red && color.red > color.blue then
                Green

            else if Debug.log "blue" color.blue > color.red && color.red > color.green then
                Blue

            else
                Red
    in
    case maxColor of
        Red ->
            { color | red = modColor <| color.red + 10, green = modColor <| color.green - 1, blue = modColor <| color.blue - 1 }

        Green ->
            { color | red = modColor <| color.red - 1, green = modColor <| color.green + 10, blue = modColor <| color.blue - 1 }

        Blue ->
            { color | red = modColor <| color.red - 1, green = modColor <| color.green - 1, blue = modColor <| color.blue + 10 }


modColor : Int -> Int
modColor c =
    modBy 12 c