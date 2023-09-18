module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Html exposing (Html, button, div, option, p, select, text, a, h1)
import Html.Attributes exposing (class, selected, value, href)
import Html.Events exposing (onClick, onInput)
import InputHelpers exposing (squareButton)
import Random
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, rx, ry, viewBox, width, x, y)
import Svg.Events
import Task


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


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { viewport = Nothing
      , colNumber = 5
      , baseColor = { red = 12, green = 12, blue = 12 }
      , rgbFunc = { redFunc = SinColor, greenFunc = ModColor, blueFunc = ModColor }
      , rgbDist = { red = Radial, green = Radial, blue = Radial }
      , origin = ( 7, 3 )
      }
    , Task.perform GotViewport getViewport
    )



---- MODEL ----


type alias Model =
    { viewport : Maybe Viewport
    , colNumber : Int
    , baseColor : DotColor
    , rgbFunc : RGBFunc
    , rgbDist : RGBDist
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


generateBaseColor : Cmd Msg
generateBaseColor =
    Random.generate RandomizeColor <| Random.int 1 12


type alias RGBFunc =
    { redFunc : ColorFunc
    , greenFunc : ColorFunc
    , blueFunc : ColorFunc
    }


type ColorFunc
    = ModColor
    | SinColor
    | CosColor
    | TanColor
    | ClampColor
    | SawColor
    | SquareColor


generateDotColor : Cmd Msg
generateDotColor =
    Random.generate RandomizeDotFunc dotFuncGenerator


dotFuncGenerator : Random.Generator RGBFunc
dotFuncGenerator =
    Random.map3 RGBFunc
        colorFuncGenerator
        colorFuncGenerator
        colorFuncGenerator


colorFuncGenerator : Random.Generator ColorFunc
colorFuncGenerator =
    Random.uniform SinColor [ ModColor, TanColor, ClampColor, CosColor, SawColor, SquareColor ]


getColor : ColorFunc -> Int -> Float
getColor colorFunc dist =
    case colorFunc of
        ModColor ->
            modBy 255 (dist * 10) |> toFloat

        SinColor ->
            255 * sin (0.1 * toFloat dist)

        CosColor ->
            255 * cos (0.1 * toFloat dist)

        TanColor ->
            255 * tan (0.1 * toFloat dist)

        ClampColor ->
            255 - (clamp 0 255 (dist * 5) |> toFloat)

        SawColor ->
            let
                amplitue =
                    255

                halfPeriod =
                    20
            in
            (amplitue / halfPeriod) * (halfPeriod - abs (modBy (2 * halfPeriod) dist - halfPeriod) |> toFloat)

        SquareColor ->
            let
                amplitude =
                    255

                halfPeriod =
                    10
            in
            if modBy (2 * halfPeriod) dist < halfPeriod then
                amplitude

            else
                0


showColorFunc : ColorFunc -> String
showColorFunc colorFunc =
    case colorFunc of
        ModColor ->
            "Modulo"

        SinColor ->
            "Sine Wave"

        CosColor ->
            "Cosine Wave"

        TanColor ->
            "Tangent Wave"

        ClampColor ->
            "Clamp"

        SawColor ->
            "Saw"

        SquareColor ->
            "Square"


colorFuncFromString : String -> ColorFunc
colorFuncFromString str =
    case str of
        "Modulo" ->
            ModColor

        "Sine Wave" ->
            SinColor

        "Cosine Wave" ->
            CosColor

        "Tangent Wave" ->
            TanColor

        "Clamp" ->
            ClampColor

        "Saw" ->
            SawColor

        "Square" ->
            SquareColor

        _ ->
            ModColor


type alias RGBDist =
    { red : DistFunc
    , green : DistFunc
    , blue : DistFunc
    }


type DistFunc
    = Radial
    | Taxi
    | Mult
    | ColOnly
    | RowOnly


generateDotDist : Cmd Msg
generateDotDist =
    Random.generate RandomizeDistFunc dotDistGenerator


dotDistGenerator : Random.Generator RGBDist
dotDistGenerator =
    Random.map3 RGBDist
        distFuncGenerator
        distFuncGenerator
        distFuncGenerator


distFuncGenerator : Random.Generator DistFunc
distFuncGenerator =
    Random.uniform Radial [ Taxi, Mult, RowOnly, ColOnly ]


type alias Dist =
    Int


getDist : DistFunc -> Row -> Col -> Dist
getDist f rowDist colDist =
    case f of
        Radial ->
            ceiling <| sqrt (toFloat (rowDist * rowDist) + toFloat (colDist * colDist))

        Taxi ->
            rowDist + colDist

        Mult ->
            rowDist * colDist

        ColOnly ->
            colDist

        RowOnly ->
            rowDist


distFuncFromString : String -> DistFunc
distFuncFromString str =
    case str of
        "Radial" ->
            Radial

        "Taxi" ->
            Taxi

        "Multiply" ->
            Mult

        "Col Only" ->
            ColOnly

        "Row Only" ->
            RowOnly

        _ ->
            Taxi


showDistFunc : DistFunc -> String
showDistFunc distFunc =
    case distFunc of
        Radial ->
            "Radial"

        Taxi ->
            "Taxi"

        Mult ->
            "Multiply"

        ColOnly ->
            "Col Only"

        RowOnly ->
            "Row Only"



---- UPDATE ----


type Msg
    = GotViewport Viewport
    | IncrementColNumber
    | DecrementColNumber
    | ClickDot DotColor Coordinate
    | RandomizeColor Int
    | RandomizeDotFunc RGBFunc
    | RandomizeDistFunc RGBDist
    | SetRedColorFunction String
    | SetGreenColorFunction String
    | SetBlueColorFunction String
    | SetRedDistFunction String
    | SetGreenDistFunction String
    | SetBlueDistFunction String
    | GetRandomColor


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
            ( { model | rgbFunc = df }, Cmd.none )

        RandomizeDistFunc df ->
            ( { model | rgbDist = df }, Cmd.none )

        GetRandomColor ->
            ( model, Cmd.batch [ generateBaseColor, generateDotColor, generateDotDist ] )

        SetRedColorFunction red ->
            let
                oldRgbFunc =
                    model.rgbFunc

                nextRgbFunc =
                    { oldRgbFunc | redFunc = colorFuncFromString red }
            in
            ( { model | rgbFunc = nextRgbFunc }, Cmd.none )

        SetGreenColorFunction green ->
            let
                oldRgbFunc =
                    model.rgbFunc

                nextRgbFunc =
                    { oldRgbFunc | greenFunc = colorFuncFromString green }
            in
            ( { model | rgbFunc = nextRgbFunc }, Cmd.none )

        SetBlueColorFunction blue ->
            let
                oldRgbFunc =
                    model.rgbFunc

                nextRgbFunc =
                    { oldRgbFunc | blueFunc = colorFuncFromString blue }
            in
            ( { model | rgbFunc = nextRgbFunc }, Cmd.none )

        SetRedDistFunction red ->
            let
                oldRgbDistFunc =
                    model.rgbDist

                nextRgbFunc =
                    { oldRgbDistFunc | red = distFuncFromString red }
            in
            ( { model | rgbDist = nextRgbFunc }, Cmd.none )

        SetGreenDistFunction green ->
            let
                oldRgbDistFunc =
                    model.rgbDist

                nextRgbFunc =
                    { oldRgbDistFunc | green = distFuncFromString green }
            in
            ( { model | rgbDist = nextRgbFunc }, Cmd.none )

        SetBlueDistFunction blue ->
            let
                oldRgbDistFunc =
                    model.rgbDist

                nextRgbFunc =
                    { oldRgbDistFunc | blue = distFuncFromString blue }
            in
            ( { model | rgbDist = nextRgbFunc }, Cmd.none )


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



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        _ ->
            Sub.none



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
        [ h1 [ class "text-gray-800 text-3xl lg:text-4xl" ] [ text "Dots" ]
        , a [ class "lnk ml-12", href "https://www.doodles.camp/" ] [ text "back" ]
        ]


subView : Model -> Html Msg
subView model =
    let
        buttonStyle =
            "mr-4 bg-transparent hover:bg-blue-500 text-blue-700 font-semibold hover:text-white py-2 px-4 border border-blue-500 hover:border-transparent rounded"
    in
    case model.viewport of
        Just viewport ->
            div [ class "w-full p-8 flex flex-col lg:flex-row h-screen" ]
                [ div [ class "flex-1 justify-center items-center" ] [ art viewport model ]
                , div [ class "flex-1" ] [ gameControls model ]
                ]

        Nothing ->
            div [] [ text "loading..." ]



---- Art ----


art : Viewport -> Model -> Html Msg
art viewport { colNumber, baseColor, origin, rgbFunc, rgbDist } =
    div [ class "full-square lg:half-square flex flex-col" ]
        (listOfDots baseColor rgbFunc rgbDist colNumber origin)


listOfDots : DotColor -> RGBFunc -> RGBDist -> Int -> Coordinate -> List (Html Msg)
listOfDots baseColor rgbFunc rgbDist count origin =
    let
        baseMatrix =
            List.repeat count (List.repeat count 0)
    in
    List.indexedMap (\rowIdx row -> rowToDots baseColor rgbFunc rgbDist rowIdx row origin) baseMatrix


rowToDots : DotColor -> RGBFunc -> RGBDist -> Int -> List a -> Coordinate -> Html Msg
rowToDots baseColor rgbFunc rgbDist rowIdx row origin =
    div [ class "flex flex-row" ]
        (List.indexedMap (\colIdx _ -> dot baseColor rgbFunc rgbDist rowIdx colIdx origin) row)


dot : DotColor -> RGBFunc -> RGBDist -> Int -> Int -> Coordinate -> Html Msg
dot { red, green, blue } { redFunc, greenFunc, blueFunc } rgbDist rowIdx colIdx origin =
    let
        distanceToRow =
            rowIdx - Tuple.first origin

        distanceToCol =
            colIdx - Tuple.second origin

        redDist =
            getDist rgbDist.red distanceToRow distanceToCol

        greenDist =
            getDist rgbDist.green distanceToRow distanceToCol

        blueDist =
            getDist rgbDist.blue distanceToRow distanceToCol

        nextRed =
            getColor redFunc redDist

        nextGreen =
            getColor greenFunc greenDist

        nextBlue =
            getColor blueFunc blueDist

        dotColor =
            DotColor red green blue

        fillColor =
            "rbga(0,0,0,1.0)"

        -- Color.fromRGB ( nextRed, nextGreen, nextBlue )
        -- |> Color.toRGBString
        wString =
            "15"
    in
    div [ class "w-full h-full flex justify-center items-center" ]
        [ svg [ Svg.Events.onClick <| ClickDot dotColor ( rowIdx, colIdx ), width wString, height wString, viewBox ("0 0 " ++ wString ++ " " ++ wString) ]
            [ rect
                [ x "0"
                , y "0"
                , fill fillColor
                , width wString
                , height wString
                ]
                []
            ]
        ]



---- Controls ----


gameControls : Model -> Html Msg
gameControls model =
    div [ class "p-4" ]
        [ div [ class "flex flex-col w-full justify-between" ]
            [ dotButtons
            , rgbColorFunctionSelector model.rgbFunc model.rgbDist
            ]
        ]


dotButtons : Html Msg
dotButtons =
    div [ class "flex flex-row w-full py-4 justify-around items-center mb-4" ]
        [ InputHelpers.addButton IncrementColNumber
        , InputHelpers.subtractButton DecrementColNumber
        , InputHelpers.resetButton GetRandomColor
        ]


rgbColorFunctionSelector : RGBFunc -> RGBDist -> Html Msg
rgbColorFunctionSelector rgbFunc rgbDist =
    div [ class "flex flex-col w-full lg:w-1/2 space-y-4" ]
        [ colorFunctionSelector rgbFunc.redFunc "Red Color" SetRedColorFunction
        , distFunctionSelector rgbDist.red "Red Distance" SetRedDistFunction
        , colorFunctionSelector rgbFunc.greenFunc "Green Color" SetGreenColorFunction
        , distFunctionSelector rgbDist.green "Green Distance" SetGreenDistFunction
        , colorFunctionSelector rgbFunc.blueFunc "Blue Color" SetBlueColorFunction
        , distFunctionSelector rgbDist.blue "Blue Distance" SetBlueDistFunction
        ]


colorFunctionSelector : ColorFunc -> String -> (String -> Msg) -> Html Msg
colorFunctionSelector currentColorFunc labelText toMsg =
    let
        f =
            showColorFunc
    in
    InputHelpers.dropDown toMsg
        currentColorFunc
        labelText
        [ InputHelpers.Option (f ModColor) (f ModColor) ModColor
        , InputHelpers.Option (f SinColor) (f SinColor) SinColor
        , InputHelpers.Option (f CosColor) (f CosColor) CosColor
        , InputHelpers.Option (f TanColor) (f TanColor) TanColor
        , InputHelpers.Option (f SawColor) (f SawColor) SawColor
        , InputHelpers.Option (f SquareColor) (f SquareColor) SquareColor
        , InputHelpers.Option (f ClampColor) (f ClampColor) ClampColor
        ]


functionSelectOption : (ColorFunc -> String) -> ColorFunc -> ColorFunc -> Html Msg
functionSelectOption toString colorFunc s =
    option [ value <| toString colorFunc, selected (colorFunc == s) ] [ text <| toString colorFunc ]


distFunctionSelector : DistFunc -> String -> (String -> Msg) -> Html Msg
distFunctionSelector currentDistFunc labelText toMsg =
    let
        f =
            showDistFunc
    in
    InputHelpers.dropDown toMsg
        currentDistFunc
        labelText
        [ InputHelpers.Option (f Radial) (f Radial) Radial
        , InputHelpers.Option (f Taxi) (f Taxi) Taxi
        , InputHelpers.Option (f Mult) (f Mult) Mult
        , InputHelpers.Option (f RowOnly) (f RowOnly) RowOnly
        , InputHelpers.Option (f ColOnly) (f ColOnly) ColOnly
        ]
