module Dots exposing (Model, Msg, init, update, view)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Color
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random
import Session exposing (WithSession)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, viewBox, width, x, y)
import Svg.Events
import Task


type alias Model =
    WithSession
        { viewport : Maybe Viewport
        , colNumber : Int
        , baseColor : DotColor
        , dotFunc : DotFunc
        , origin : Coordinate
        }


type alias Coordinate =
    ( Row, Col )


type alias Row =
    Int


type alias Col =
    Int


showCoordinate : Coordinate -> String
showCoordinate coordinate =
    "(" ++ (String.fromInt <| Tuple.first coordinate) ++ "," ++ (String.fromInt <| Tuple.second coordinate) ++ ")"


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


type alias DotFunc =
    { redFunc : ColorFunc
    , greenFunc : ColorFunc
    , blueFunc : ColorFunc
    }


type Msg
    = GotViewport Viewport
    | IncrementColNumber
    | DecrementColNumber
    | ClickDot DotColor Coordinate
    | RandomizeColor Int
    | RandomizeDotFunc DotFunc
    | GetRandomColor


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , viewport = Nothing
      , colNumber = 50
      , baseColor = { red = 12, green = 12, blue = 12 }
      , dotFunc = { redFunc = ModColor, greenFunc = ModColor, blueFunc = ModColor }
      , origin = ( 7, 3 )
      }
    , Task.perform GotViewport getViewport
    )


generateBaseColor : Cmd Msg
generateBaseColor =
    Random.generate RandomizeColor <| Random.int 1 12


generateDotColor : Cmd Msg
generateDotColor =
    Random.generate RandomizeDotFunc dotFuncGenerator


dotFuncGenerator : Random.Generator DotFunc
dotFuncGenerator =
    Random.map3 DotFunc
        colorFuncGenerator
        colorFuncGenerator
        colorFuncGenerator


colorFuncGenerator : Random.Generator ColorFunc
colorFuncGenerator =
    Random.uniform SinColor [ ModColor ]


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
                , div [] [ text <| showCoordinate model.origin ]
                , art viewport model
                ]

        Nothing ->
            div [] [ text "loading..." ]


art : Viewport -> Model -> Html Msg
art viewport { colNumber, baseColor, origin, dotFunc } =
    let
        { width, height } =
            viewport.viewport

        m =
            min width height

        w =
            ceiling <| (m + 100) / toFloat (colNumber * 2)
    in
    div [ class "grid dot-box m-auto border-2" ] (listOfDots baseColor dotFunc w colNumber origin)


listOfDots : DotColor -> DotFunc -> Int -> Int -> Coordinate -> List (Html Msg)
listOfDots baseColor dotFunc w count origin =
    let
        baseMatrix =
            List.repeat count (List.repeat count 0)
    in
    List.indexedMap (\rowIdx row -> rowToDots baseColor dotFunc w rowIdx row origin) baseMatrix


rowToDots : DotColor -> DotFunc -> Int -> Int -> List a -> Coordinate -> Html Msg
rowToDots baseColor dotFunc w rowIdx row origin =
    div [ class "flex flex-row" ]
        (List.indexedMap (\colIdx _ -> dot baseColor dotFunc w rowIdx colIdx origin) row)



---- Color Functions ---


sinColor : Int -> Float
sinColor dist =
    255 * sin (0.5 * toFloat dist)


modColor : Int -> Float
modColor dist =
    modBy 255 (dist * 20) |> toFloat


getRed : ColorFunc -> Int -> Float
getRed colorFunc dist =
    showModColor colorFunc dist


getGreen : ColorFunc -> Int -> Float
getGreen colorFunc dist =
    showModColor colorFunc dist


getBlue : ColorFunc -> Int -> Float
getBlue colorFunc dist =
    showModColor colorFunc dist


showModColor : ColorFunc -> (Int -> Float)
showModColor func =
    case func of
        ModColor ->
            modColor

        SinColor ->
            sinColor


type ColorFunc
    = ModColor
    | SinColor


radialDist : Int -> Int -> Int
radialDist row col =
    ceiling <| sqrt (toFloat (row * row) + toFloat (col * col))


dot : DotColor -> DotFunc -> Int -> Int -> Int -> Coordinate -> Html Msg
dot { red, green, blue } { redFunc, greenFunc, blueFunc } w rowIdx colIdx origin =
    let
        distanceToRow =
            rowIdx - Tuple.first origin

        distanceToCol =
            colIdx - Tuple.second origin

        radDist =
            radialDist distanceToCol distanceToRow

        addDist =
            distanceToCol + distanceToRow

        multDist =
            distanceToCol * distanceToRow

        nextRed =
            getRed redFunc radDist

        nextGreen =
            getGreen greenFunc addDist

        nextBlue =
            getBlue blueFunc multDist

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
        [ svg [ Svg.Events.onClick <| ClickDot dotColor ( rowIdx, colIdx ), width wString, height wString, viewBox ("0 0 " ++ wString ++ " " ++ wString) ]
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        IncrementColNumber ->
            ( { model | colNumber = model.colNumber + 1 }, Cmd.none )

        DecrementColNumber ->
            ( { model | colNumber = model.colNumber - 1 }, Cmd.none )

        ClickDot oldColor origin ->
            let
                nextColor =
                    getNextColor oldColor
            in
            ( { model | baseColor = nextColor, origin = origin }, Cmd.none )

        RandomizeColor color ->
            let
                nextColor =
                    DotColor color 12 12
            in
            ( { model | baseColor = nextColor }, Cmd.none )

        RandomizeDotFunc df ->
            ( { model | dotFunc = df }, Cmd.none )

        GetRandomColor ->
            ( model, Cmd.batch [ generateBaseColor, generateDotColor ] )


getNextColor : DotColor -> DotColor
getNextColor color =
    let
        maxColor =
            if color.red > color.green && color.green > color.blue then
                Red

            else if color.green > color.red && color.red > color.blue then
                Green

            else if color.blue > color.red && color.red > color.green then
                Blue

            else
                Red
    in
    case maxColor of
        Red ->
            { color | red = modBaseColor <| color.red + 10, green = modBaseColor <| color.green - 1, blue = modBaseColor <| color.blue - 1 }

        Green ->
            { color | red = modBaseColor <| color.red - 1, green = modBaseColor <| color.green + 10, blue = modBaseColor <| color.blue - 1 }

        Blue ->
            { color | red = modBaseColor <| color.red - 1, green = modBaseColor <| color.green - 1, blue = modBaseColor <| color.blue + 10 }


modBaseColor : Int -> Int
modBaseColor c =
    modBy 12 c
