module Doodle.ModularTimesTable.Main exposing (Model, Msg, init, update, view)

import Color
import Color.Interpolate
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)
import InputHelpers exposing (..)
import Session exposing (WithSession)
import Svg exposing (Svg)
import Svg.Attributes


type alias Radian =
    Float


type alias Coordinate =
    ( Float, Float )


maxModulus : number
maxModulus =
    5003


type alias Model =
    WithSession
        { multiplier : Int
        , modulus : Int
        }


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , multiplier = 185
      , modulus = 1656
      }
    , Cmd.none
    )


type Msg
    = UpdateModulus String
    | UpdateMultiplier String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateModulus modulus ->
            let
                maybeNextModulus =
                    String.toInt modulus
            in
            case maybeNextModulus of
                Just nextModulus ->
                    ( { model
                        | modulus = min nextModulus maxModulus
                        , multiplier = min model.multiplier (nextModulus - 1)
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        UpdateMultiplier multiplier ->
            let
                maybeNextMultiplier =
                    String.toInt multiplier
            in
            case maybeNextMultiplier of
                Just nextMultiplier ->
                    ( { model | multiplier = min nextMultiplier (model.modulus - 1) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "space-y-4" ]
        [ header
        , div [ class "flex flex-col space-y-8 lg:space-y-0 lg:flex-row lg:space-x-8 lg:h-[80vh]" ]
            [ inputs model
            , timesTable model
            ]
        , footer
        ]


header : Html Msg
header =
    h1 [ class "h1" ] [ text "modular times table" ]


inputs : Model -> Html Msg
inputs { modulus, multiplier } =
    let
        modulusText =
            String.fromInt modulus

        multiplierText =
            String.fromInt multiplier

        modulusOptionText =
            String.join " " [ "max:", String.fromInt maxModulus ]
    in
    div [ class "flex space-x-2 justify-start lg:flex-col lg:space-x-0 lg:space-y-2" ]
        [ InputHelpers.numberInput "modulus" modulusOptionText "-" modulusText UpdateModulus
        , InputHelpers.numberInput "multiplier" "max: (mod - 1)" "-" multiplierText UpdateMultiplier
        ]


buildModulusIntList : Int -> List Int
buildModulusIntList modulus =
    List.range 0 (modulus - 1)


buildRemainersList : Int -> Int -> List Int -> List Int
buildRemainersList multiplier modulus modulusIntList =
    modulusIntList
        |> List.map (\v -> v * multiplier)
        |> List.map (remainderBy modulus)


timesTable : Model -> Html Msg
timesTable model =
    let
        modulusIntList =
            buildModulusIntList model.modulus

        remaindersList =
            buildRemainersList model.multiplier model.modulus modulusIntList

        zippedList =
            List.map2 (\m r -> ( m, r )) modulusIntList remaindersList
    in
    div [ class "w-full" ]
        [ numberDiagram model.modulus zippedList
        ]


numberDiagram : Int -> List ( Int, Int ) -> Html Msg
numberDiagram modulus zippedList =
    let
        radius =
            1500

        width =
            String.fromInt (radius * 2)

        height =
            String.fromInt (radius * 2)

        viewBox =
            String.join " " [ "0", "0", width, height ]
    in
    Svg.svg
        [ Svg.Attributes.width width
        , Svg.Attributes.height height
        , Svg.Attributes.viewBox viewBox
        , Svg.Attributes.class "w-full h-full"
        ]
        (buildLines radius modulus zippedList)


buildLines : Float -> Int -> List ( Int, Int ) -> List (Svg Msg)
buildLines radius modulus pairs =
    let
        linePoints =
            buildLinePoints radius modulus pairs
    in
    List.map (pointsToSvgLine radius) linePoints


pointsToSvgLine : Float -> ( Coordinate, Coordinate ) -> Svg Msg
pointsToSvgLine radius ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        length =
            sqrt <| (x2 - x1) ^ 2 + (y2 - y1) ^ 2

        color =
            lengthToColor (2 * radius) length
    in
    Svg.line
        [ Svg.Attributes.x1 <| String.fromFloat x1
        , Svg.Attributes.x2 <| String.fromFloat x2
        , Svg.Attributes.y1 <| String.fromFloat y1
        , Svg.Attributes.y2 <| String.fromFloat y2
        , Svg.Attributes.stroke color
        , Svg.Attributes.width "1"
        ]
        []


lengthToColor : Float -> Float -> String
lengthToColor maxLength length =
    let
        percent =
            length / maxLength

        startColor =
            Color.red

        endColor =
            Color.blue

        color =
            Color.Interpolate.interpolate Color.Interpolate.RGB startColor endColor percent
    in
    Color.toCssString color


buildLinePoints : Float -> Int -> List ( Int, Int ) -> List ( Coordinate, Coordinate )
buildLinePoints radius modulus pairs =
    let
        toCartesian =
            modAndIntToCartesian radius modulus

        result =
            pairs
                |> List.map (Tuple.mapBoth toCartesian toCartesian)
    in
    result


modAndIntToCartesian : Float -> Int -> Int -> Coordinate
modAndIntToCartesian radius totalSegments countSegment =
    let
        radian =
            modAndIntToAngle totalSegments countSegment
    in
    radianAndLengthToCartesian radius radian


modAndIntToAngle : Int -> Int -> Radian
modAndIntToAngle totalSegments countSegment =
    let
        radiansPerSegment =
            2 * pi / toFloat totalSegments
    in
    (toFloat countSegment * radiansPerSegment) - (pi / 2)


radianAndLengthToCartesian : Float -> Radian -> Coordinate
radianAndLengthToCartesian length radian =
    let
        x =
            length * cos radian + length

        y =
            length * sin radian + length
    in
    ( x, y )


footer : Html Msg
footer =
    div [ class "flex flex-col pt-8 lg:flex-row lg:space-x-2 lg:pt-0" ]
        [ div []
            [ text "inspired by: "
            , a [ class "lnk", href "https://youtu.be/6ZrO90AI0c8" ] [ text "vortex math" ]
            ]
        , div [ class "hidden lg:block" ] [ text "|" ]
        , div []
            [ text "github: "
            , a [ class "lnk", href "https://github.com/johnschoeman/doodles" ]
                [ text "/doodles" ]
            ]
        ]
