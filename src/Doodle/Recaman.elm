module Doodle.Recaman exposing (Model, Msg, init, update, view)

import Browser.Dom exposing (Viewport, getViewport)
import Html exposing (Html, div, input, text)
import Html.Attributes as Attr exposing (class, style, type_, value)
import Html.Events exposing (onInput)
import InputHelpers
import Seq exposing (Seq)
import Session exposing (WithSession)
import Set exposing (Set)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, stroke, strokeWidth, viewBox)
import Task


type alias Model =
    WithSession
        { viewport : Maybe Viewport
        , scale : Scale
        , count : Int
        , root : Int
        , sweepPattern : SweepPattern
        }


type SweepPattern
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


showPattern : SweepPattern -> String
showPattern pattern =
    case pattern of
        A ->
            "Butts"

        B ->
            "Waves"

        C ->
            "Pure"

        D ->
            "UnderParallels"

        E ->
            "InvertedButts"

        F ->
            "OverParallels"

        G ->
            "Tails"

        H ->
            "OverWaves"


patternFromString : String -> SweepPattern
patternFromString pattern =
    case pattern of
        "Butts" ->
            A

        "Waves" ->
            B

        "Pure" ->
            C

        "UnderParallels" ->
            D

        "InvertedButts" ->
            E

        "OverParallels" ->
            F

        "Tails" ->
            G

        "OverWaves" ->
            H

        _ ->
            A


type alias IncDec =
    Bool


type alias OddIndex =
    Bool


type alias OnDirectionChange =
    Bool


toFoo : SweepPattern -> ( IncDec, OddIndex, OnDirectionChange )
toFoo sweep =
    case sweep of
        A ->
            ( True, True, True )

        B ->
            ( True, False, True )

        C ->
            ( False, True, True )

        D ->
            ( False, False, True )

        E ->
            ( True, True, False )

        F ->
            ( True, False, False )

        G ->
            ( False, True, False )

        H ->
            ( False, False, False )


type alias Scale =
    Int


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , viewport = Nothing
      , scale = 2
      , count = 400
      , root = 17
      , sweepPattern = G
      }
    , Task.perform GotViewport getViewport
    )


sequenceToPairs : List Int -> List ( Int, Int )
sequenceToPairs seq =
    let
        h =
            List.head seq

        next =
            List.tail seq
    in
    case ( h, next ) of
        ( Just hd, Just nextSeq ) ->
            let
                nextHead =
                    List.head nextSeq
            in
            case nextHead of
                Just nextH ->
                    ( hd, nextH ) :: sequenceToPairs nextSeq

                Nothing ->
                    []

        ( _, _ ) ->
            []


generateRecaman : Int -> Int -> Int -> List Int
generateRecaman n root count =
    let
        seq =
            recamanSeq count n root Set.empty Seq.empty

        nTerms =
            Seq.take count seq
    in
    Seq.toList nTerms


recamanSeq : Int -> Int -> Int -> Set Int -> Seq Int -> Seq Int
recamanSeq count n lastTerm visited soFar =
    let
        minusTerm =
            lastTerm - n

        plusTerm =
            lastTerm + n
    in
    if n < count then
        if minusTerm >= 0 && not (Set.member minusTerm visited) then
            let
                nextSet =
                    Set.insert minusTerm visited

                currentSeq =
                    Seq.cons minusTerm soFar
            in
            recamanSeq count (n + 1) minusTerm nextSet currentSeq

        else
            let
                nextSet =
                    Set.insert plusTerm visited

                currentSeq =
                    Seq.cons plusTerm soFar
            in
            recamanSeq count (n + 1) plusTerm nextSet currentSeq

    else
        Seq.reverse soFar


type Msg
    = GotViewport Viewport
    | UpdateCount Int
    | UpdateRoot Int
    | UpdateScale Int
    | UpdateSweepPattern String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        UpdateCount count ->
            ( { model | count = count }, Cmd.none )

        UpdateRoot root ->
            ( { model | root = root }, Cmd.none )

        UpdateScale scale ->
            ( { model | scale = scale }, Cmd.none )

        UpdateSweepPattern pattern ->
            ( { model | sweepPattern = patternFromString pattern }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.viewport of
        Just viewport ->
            div []
                [ artInputs model
                , art viewport model
                ]

        Nothing ->
            div [] [ text "loading..." ]


artInputs : Model -> Html Msg
artInputs model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "space-around"
        , style "height" "140px"
        , style "width" "400px"
        , style "border" "1px solid gray"
        , style "padding" "16px"
        , style "position" "fixed"
        ]
        [ slider 0 1000 model.count UpdateCount
        , slider 0 111 model.root UpdateRoot
        , slider 1 17 model.scale UpdateScale
        , drawSettingDropdown model.sweepPattern UpdateSweepPattern
        ]


slider : Int -> Int -> Int -> (Int -> msg) -> Html msg
slider minValue maxValue n toMsg =
    let
        f v =
            case String.toInt v of
                Just i ->
                    toMsg i

                Nothing ->
                    toMsg 0
    in
    input
        [ type_ "range"
        , Attr.min <| String.fromInt minValue
        , Attr.max <| String.fromInt maxValue
        , value <| String.fromInt n
        , onInput f
        ]
        []


drawSettingDropdown : SweepPattern -> (String -> Msg) -> Html Msg
drawSettingDropdown currentPattern toMsg =
    InputHelpers.dropDown toMsg
        currentPattern
        "Pattern"
        [ InputHelpers.Option (showPattern A) (showPattern A) A
        , InputHelpers.Option (showPattern B) (showPattern B) B
        , InputHelpers.Option (showPattern C) (showPattern C) C
        , InputHelpers.Option (showPattern D) (showPattern D) D
        , InputHelpers.Option (showPattern E) (showPattern E) E
        , InputHelpers.Option (showPattern F) (showPattern F) F
        , InputHelpers.Option (showPattern G) (showPattern G) G
        , InputHelpers.Option (showPattern H) (showPattern H) H
        ]


art : Viewport -> Model -> Html msg
art viewport { sweepPattern, count, root, scale } =
    let
        recaman =
            generateRecaman 0 root count

        -- [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
        -- [ 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 ]
        -- [ 0, 10, 9, 8, 11, 12, 13, 4, 3, 2, 1, 0 ]
        -- [1, 2, 3, 4]
        { width, height } =
            viewport.viewport

        m =
            min width height

        mString =
            String.fromInt <| ceiling m

        pairs =
            sequenceToPairs recaman

        pairsWithScale =
            applyScale scale pairs

        startY =
            400
    in
    div [ class "border bg-gray-100" ]
        [ svg [ viewBox <| String.join " " [ "0 0", mString, mString ] ]
            [ path
                [ d <| makeSvgPath sweepPattern startY pairsWithScale
                , stroke "black"
                , strokeWidth "2"
                , fill "pink"
                ]
                []
            ]
        ]


makeSvgPath : SweepPattern -> Int -> List ( Int, Int ) -> String
makeSvgPath sweepPattern startY pairs =
    let
        maybeFirstPoint =
            List.head pairs
    in
    case maybeFirstPoint of
        Just ( startX, _ ) ->
            pathDrawing
                (startPoint startX startY
                    :: makeHalfCirlces sweepPattern startY pairs
                )

        Nothing ->
            ""


pathDrawing : List String -> String
pathDrawing parts =
    String.join " " parts


startPoint : Int -> Int -> String
startPoint x y =
    String.join " " [ "M", String.fromInt x, String.fromInt y ]


applyScale : Int -> List ( Int, Int ) -> List ( Int, Int )
applyScale scale seq =
    List.map (\t -> Tuple.mapBoth (\x -> x * scale) (\x -> x * scale) t) seq


makeHalfCirlces : SweepPattern -> Int -> List ( Int, Int ) -> List String
makeHalfCirlces sweepPattern y points =
    List.indexedMap (\idx ( l, r ) -> buildCircle sweepPattern y l r idx) points


buildCircle : SweepPattern -> Int -> Int -> Int -> Int -> String
buildCircle sweepPattern y l r idx =
    case toFoo sweepPattern of
        ( incDec, idxOdd, onDir ) ->
            let
                sweep =
                    sweepDirection incDec idxOdd l r idx
            in
            if onDir then
                if l < r then
                    halfCircle y l r sweep

                else
                    halfCircle y r l sweep

            else
                halfCircle y l r sweep


sweepDirection : IncDec -> OddIndex -> Int -> Int -> Int -> Bool
sweepDirection incDec idxOdd left right idx =
    let
        forward =
            right - left > 0

        odd =
            modBy 2 idx == 1
    in
    case ( incDec, idxOdd ) of
        ( True, True ) ->
            forward && odd

        ( True, False ) ->
            forward

        ( False, True ) ->
            odd

        ( False, False ) ->
            True


halfCircle : Int -> Int -> Int -> Bool -> String
halfCircle startY startX endX orientation =
    let
        radius =
            (endX - startX) // 2

        sweepFlag =
            if orientation then
                1

            else
                0
    in
    String.join " " [ point startX startY, arc radius radius 0 sweepFlag endX startY ]



-- String.join " " [ point endX startY, arc radius radius 0 sweepFlag startX startY ]
-- String.join " " [ arc radius radius 0 sweepFlag startX startY ]
-- String.join " " [ arc radius radius 0 sweepFlag endX startY ]


point : Int -> Int -> String
point x y =
    String.join " "
        -- ("M" :: List.map String.fromInt [ 200, y ])
        ("M" :: List.map String.fromInt [ x, y ])


arc : Int -> Int -> Int -> Int -> Int -> Int -> String
arc rx ry xRot sweepFlag endX endY =
    let
        pieces =
            [ rx, ry, xRot, 0, sweepFlag, endX, endY ]

        strings =
            "A" :: List.map String.fromInt pieces
    in
    String.join " " strings
