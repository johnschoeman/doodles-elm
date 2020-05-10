module Doodle.Recaman exposing (Model, Msg, init, update, view)

import Browser.Dom exposing (Viewport, getViewport)
import Html exposing (Html, div, input, text)
import Html.Attributes as Attr exposing (class, style, type_)
import Html.Events exposing (onInput)
import Seq exposing (Seq)
import Session exposing (WithSession)
import Set exposing (Set)
import Svg exposing (path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, stroke, strokeWidth, viewBox)
import Task


type alias Model =
    WithSession
        { viewport : Maybe Viewport
        , scale : Scale
        , count : Int
        , root : Int
        }


type alias Recaman =
    List Int


type alias Scale =
    Int


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , viewport = Nothing
      , scale = 10
      , count = 100
      , root = 1
      }
    , Task.perform GotViewport getViewport
    )


sequenceToPairs : List Int -> List ( Int, Int )
sequenceToPairs seq =
    -- [1, 2, 3, 4] -> [(1, 2), (2, 3), (3, 4)]
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



-- [ 1, 2, 4 ]


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



-- Seq.cons plusTerm Seq.numbers
--   @JSExport
--   def generateRecaman(n: Int, root: Int, count: Int) = {
--     val list = recaman(n, root, Set()).take(count).toList
--     list.asJson.noSpaces
--   }
--
--   @JSExport
--   def recaman(n: Int, lastTerm: Int, visited: Set[Int]): Stream[Int] = {
--     val minusTerm: Int = lastTerm - n
--     if (minusTerm >= 0 && !visited.contains(minusTerm)) {
--       minusTerm #:: recaman(n + 1, minusTerm, visited.+(minusTerm))
--     } else {
--       val plusTerm: Int = lastTerm + n
--       plusTerm #:: recaman(n + 1, plusTerm, visited + plusTerm)
--     }
--   }
-- }


type Msg
    = GotViewport Viewport
    | UpdateCount Int
    | UpdateRoot Int
    | UpdateScale Int


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
        , style "height" "80px"
        , style "width" "200px"
        ]
        [ slider 0 1000 model.count UpdateCount
        , slider 0 111 model.count UpdateRoot
        , slider 1 17 model.count UpdateScale
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
        , onInput f
        ]
        []


art : Viewport -> Model -> Html msg
art viewport { count, root, scale } =
    let
        recaman =
            generateRecaman 0 root count

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
                [ d <| makeSvgPath startY pairsWithScale
                , stroke "black"
                , strokeWidth "1"
                , fill "white"
                ]
                []
            ]
        ]


makeSvgPath : Int -> List ( Int, Int ) -> String
makeSvgPath startY pairs =
    let
        maybeFirstPoint =
            List.head pairs
    in
    case maybeFirstPoint of
        Just ( startX, _ ) ->
            pathDrawing
                (startPoint startX startY
                    :: makeHalfCirlces startY pairs
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


makeHalfCirlces : Int -> List ( Int, Int ) -> List String
makeHalfCirlces y points =
    List.indexedMap (\idx ( l, r ) -> halfCircle y l r (sweepDirection l r idx)) points


sweepDirection : Int -> Int -> Int -> Bool
sweepDirection left right idx =
    let
        forward =
            right - left > 0

        odd =
            modBy 2 idx == 1
    in
    odd



-- odd
-- forward
-- odd
-- forward && odd
-- True


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
    arc radius radius 0 sweepFlag endX startY


arc : Int -> Int -> Int -> Int -> Int -> Int -> String
arc rx ry xRot sweepFlag endX endY =
    let
        pieces =
            [ rx, ry, xRot, 0, sweepFlag, endX, endY ]

        strings =
            "A" :: List.map String.fromInt pieces
    in
    String.join " " strings
