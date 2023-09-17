module GameScreen exposing (Model, Msg, init, update, view)

import Board as Board exposing (Board, Node(..), Status(..), board1, boardDictionary, getDataAtNode, getNeighborNode, updateBoardByNode)
import Dict
import FeatherIcons
import GameEngine as GameEngine
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Level as Level exposing (Level)
import Svg exposing (Svg, circle, line, svg)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, strokeWidth, viewBox, x1, x2, y1, y2)
import Svg.Events



---- MODEL ----


type alias Selection =
    Node


type alias Model =
    { selection : Selection
    , levelId : Int
    , levels : List Level
    , board : Board
    }


init : Int -> List Level -> Model
init levelId levels =
    let
        board =
            case Level.getLevel levelId levels of
                Just l ->
                    l.board

                Nothing ->
                    Board.board1
    in
    { selection = A
    , levelId = levelId
    , levels = levels
    , board = board
    }



---- UPDATE ----


type Msg
    = SelectNode Node
    | ResetGame
    | IncrementLevel
    | DecrementLevel
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectNode toNode ->
            let
                fromNode =
                    model.selection

                nextBoard =
                    GameEngine.update fromNode toNode model.board

                nextLevels =
                    Level.updateLevels (GameEngine.gameState nextBoard) model.levelId model.levels
            in
            { model
                | selection = toNode
                , board = nextBoard
                , levels = nextLevels
            }

        ResetGame ->
            resetGame model

        IncrementLevel ->
            let
                nextLevelId =
                    model.levelId + 1

                maybeNextBoard =
                    Dict.get nextLevelId Board.boardDictionary
            in
            case maybeNextBoard of
                Just board ->
                    { model | levelId = nextLevelId, board = board }

                Nothing ->
                    model

        DecrementLevel ->
            let
                nextLevelId =
                    model.levelId - 1

                maybeNextBoard =
                    Dict.get nextLevelId Board.boardDictionary
            in
            case maybeNextBoard of
                Just board ->
                    { model | levelId = nextLevelId, board = board }

                Nothing ->
                    model

        NoOp ->
            model


resetGame : Model -> Model
resetGame model =
    let
        maybeBoard =
            Dict.get model.levelId Board.boardDictionary
    in
    case maybeBoard of
        Just board ->
            { model | board = board }

        Nothing ->
            { model | board = Board.board1 }



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "w-full h-full flex flex-col lg:flex-row space-y-8 mt-8" ]
        [ div [ class "w-full h-full" ]
            [ boardToSvg model.board model.selection
            ]
        , gameControls model
        ]


gameControls : Model -> Html Msg
gameControls model =
    div [ class "flex flex-col justify-center lg:justify-start items-center w-full space-y-8 lg:max-w-sm p-4" ]
        [ div [ class "" ] [ gameButtons model ]
        , div [ class "border-b border-gray-400 w-full" ] []
        , div [ class "flex justify-center items-center" ] [ gameStateIcon model.board ]
        ]


gameStateIcon : Board -> Html Msg
gameStateIcon board =
    case GameEngine.gameState board of
        GameEngine.Won ->
            div [ class "text-green-600" ] [ winIcon ]

        GameEngine.Lost ->
            div [ class "text-red-600" ] [ failIcon ]

        GameEngine.InProgress ->
            inProgressIcon


gameButtons : Model -> Html Msg
gameButtons model =
    let
        buttonStyle =
            "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-2 rounded"

        currentLevelText =
            String.fromInt model.levelId
    in
    div [ class "w-full flex flex-row justify-between lg:justify-around" ]
        [ div [ class "flex flex-row" ]
            [ div [ class "flex-1 mr-2" ]
                [ button [ onClick DecrementLevel, class buttonStyle ] [ leftIcon ]
                ]
            , div [ class "flex-1 text-2xl font-bold" ] [ text currentLevelText ]
            , div [ class "flex-1 ml-2" ]
                [ button [ onClick IncrementLevel, class buttonStyle ] [ rightIcon ]
                ]
            ]
        , div [ class "" ]
            [ button [ onClick ResetGame, class (buttonStyle ++ " ml-8") ] [ resetIcon ]
            ]
        ]


winIcon : Html Msg
winIcon =
    FeatherIcons.checkCircle |> FeatherIcons.withSize 64 |> FeatherIcons.toHtml []


failIcon : Html Msg
failIcon =
    FeatherIcons.xCircle |> FeatherIcons.withSize 64 |> FeatherIcons.toHtml []


inProgressIcon : Html Msg
inProgressIcon =
    FeatherIcons.circle |> FeatherIcons.withSize 64 |> FeatherIcons.toHtml []


leftIcon : Html Msg
leftIcon =
    FeatherIcons.arrowLeft |> FeatherIcons.toHtml []


rightIcon : Html Msg
rightIcon =
    FeatherIcons.arrowRight |> FeatherIcons.toHtml []


resetIcon : Html Msg
resetIcon =
    FeatherIcons.refreshCw |> FeatherIcons.toHtml []


boardToSvg : Board -> Node -> Html Msg
boardToSvg board selection =
    let
        cellWithSelection =
            cellToSvg selection

        scaleFactor =
            40

        row1 =
            String.fromInt (scaleFactor * 1)

        row2 =
            String.fromInt (scaleFactor * 2)

        row3 =
            String.fromInt (scaleFactor * 3)

        row4 =
            String.fromInt (scaleFactor * 4)

        row5 =
            String.fromInt (scaleFactor * 5)

        col1 =
            String.fromInt (scaleFactor * 1)

        col2 =
            String.fromInt (scaleFactor * 2)

        col3 =
            String.fromInt (scaleFactor * 3)

        col4 =
            String.fromInt (scaleFactor * 4)

        col5 =
            String.fromInt (scaleFactor * 5)

        viewWidth =
            String.fromInt (scaleFactor * 6)

        viewBoxAttribute =
            "0 0 " ++ viewWidth ++ " " ++ viewWidth
    in
    svg [ viewBox viewBoxAttribute, strokeWidth "3", stroke "black" ]
        [ -- long diagonals
          line [ x1 row1, x2 row5, y1 col1, y2 col5, stroke "black" ] []
        , line [ x1 row5, x2 row1, y1 col1, y2 col5, stroke "black" ] []

        -- short diagonals
        , line [ x1 row1, x2 row3, y1 col3, y2 col5, stroke "black" ] []
        , line [ x1 row3, x2 row1, y1 col1, y2 col3, stroke "black" ] []
        , line [ x1 row3, x2 row5, y1 col1, y2 col3, stroke "black" ] []
        , line [ x1 row3, x2 row5, y1 col5, y2 col3, stroke "black" ] []

        -- horizontals
        , line [ x1 row1, x2 row1, y1 col1, y2 col5, stroke "black" ] []
        , line [ x1 row3, x2 row3, y1 col1, y2 col5, stroke "black" ] []
        , line [ x1 row5, x2 row5, y1 col1, y2 col5, stroke "black" ] []
        , line [ x1 row1, x2 row5, y1 col1, y2 col1, stroke "black" ] []
        , line [ x1 row1, x2 row5, y1 col3, y2 col3, stroke "black" ] []
        , line [ x1 row1, x2 row5, y1 col5, y2 col5, stroke "black" ] []
        , cellWithSelection A board.a col1 row1
        , cellWithSelection B board.b col3 row1
        , cellWithSelection C board.c col5 row1
        , cellWithSelection D board.d col2 row2
        , cellWithSelection E board.e col4 row2
        , cellWithSelection F board.f col1 row3
        , cellWithSelection G board.g col3 row3
        , cellWithSelection H board.h col5 row3
        , cellWithSelection I board.i col2 row4
        , cellWithSelection J board.j col4 row4
        , cellWithSelection K board.k col1 row5
        , cellWithSelection L board.l col3 row5
        , cellWithSelection M board.m col5 row5
        ]


cellToSvg : Node -> Node -> Status -> String -> String -> Html Msg
cellToSvg selection cellNode cellStatus xPos yPos =
    let
        circleFill =
            case cellStatus of
                Dot ->
                    fill "blue"

                BlackDot ->
                    fill "black"

                Empty ->
                    fill "white"

        circleStroke =
            if selection == cellNode then
                stroke "green"

            else
                stroke "black"
    in
    svg
        [ Svg.Events.onClick (SelectNode cellNode)
        , circleFill
        , circleStroke
        ]
        [ circle [ cx xPos, cy yPos, r "15" ] []
        ]
