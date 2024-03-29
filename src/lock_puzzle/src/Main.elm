module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import FeatherIcons
import Html exposing (Html, a, button, div, h1, input, label, text)
import Html.Attributes as Attr exposing (class, classList, href, style, type_, value)
import Html.Events exposing (onClick, onInput)
import InputHelpers
import List.Extra exposing (updateAt)
import Random
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



---- MODEL ----


type alias Model =
    { viewport : Maybe Viewport
    , board : Board
    , moveCount : Int
    , boardSize : Int
    }


type alias Board =
    List (List Cell)


type Cell
    = On
    | Off


buildBoard : Int -> Board
buildBoard size =
    List.repeat size (buildRow size)


buildRow : Int -> List Cell
buildRow size =
    List.repeat size Off


randomBoard : Int -> Random.Generator Board
randomBoard boardSize =
    Random.list boardSize (Random.list boardSize randomCell)


randomCell : Random.Generator Cell
randomCell =
    Random.uniform On [ Off ]


generateBoard : Int -> Cmd Msg
generateBoard boardSize =
    Random.generate ResetGame (randomBoard boardSize)


initialBoard : Board
initialBoard =
    buildBoard 4


threeByThreeBoard : Board
threeByThreeBoard =
    [ [ Off, On, Off ], [ On, On, On ], [ On, On, On ] ]


fiveByFiveBoard : Board
fiveByFiveBoard =
    [ [ On, On, On, On, On ]
    , [ On, On, On, On, On ]
    , [ On, On, On, On, On ]
    , [ On, On, On, On, On ]
    , [ On, On, On, On, On ]
    ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { viewport = Nothing
      , board = initialBoard
      , moveCount = 0
      , boardSize = 4
      }
    , Cmd.batch [ Task.perform GotViewport getViewport, generateBoard 4 ]
    )



---- UPDATE ----


type Msg
    = GotViewport Viewport
    | ResetGame Board
    | GetRandomBoard
    | ClickedCell Int Int
    | UpdateBoardSize Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        ResetGame board ->
            ( { model | board = board, moveCount = 0 }, Cmd.none )

        GetRandomBoard ->
            ( model, generateBoard model.boardSize )

        ClickedCell row col ->
            let
                nextBoard =
                    updateBoard row col model.board

                nextMoveCount =
                    model.moveCount + 1
            in
            ( { model | board = nextBoard, moveCount = nextMoveCount }, Cmd.none )

        UpdateBoardSize size ->
            ( { model | boardSize = size }, generateBoard size )


updateBoard : Int -> Int -> Board -> Board
updateBoard clickedRowIdx clickedColIdx board =
    List.indexedMap (\rowIdx row -> updateRow clickedRowIdx clickedColIdx rowIdx row) board


updateRow : Int -> Int -> Int -> List Cell -> List Cell
updateRow clickedRowIdx clickedColIdx rowIdx row =
    List.indexedMap (\colIdx cell -> updateCell clickedRowIdx clickedColIdx rowIdx colIdx cell) row


updateCell : Int -> Int -> Int -> Int -> Cell -> Cell
updateCell clickedRowIdx clickedColIdx rowIdx colIdx cell =
    if clickedRowIdx == rowIdx || clickedColIdx == colIdx then
        case cell of
            On ->
                Off

            Off ->
                On

    else
        cell


isOn : Cell -> Bool
isOn cell =
    cell == On


solved : Board -> Bool
solved board =
    List.all (\row -> List.all isOn row) board



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        _ ->
            Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Lock Puzzle"
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
        [ h1 [ class "text-gray-800 text-3xl lg:text-4xl" ] [ text "Lock Puzzle" ]
        , a [ class "lnk ml-12", href "https://www.doodles.camp/" ] [ text "back" ]
        ]


subView : Model -> Html Msg
subView model =
    div [ class "w-full flex flex-col lg:flex-row h-[80vh] mt-4" ]
        [ div [ class "flex-1 flex justify-center" ] [ gameBoard model ]
        , div [ class "flex-1" ] [ gameControls model ]
        ]



---- Board ----


gameBoard : Model -> Html Msg
gameBoard model =
    div [ class "full-square lg:half-square flex flex-col" ] (boardRows model.board)


boardRows : Board -> List (Html Msg)
boardRows board =
    List.indexedMap (\rowIdx row -> rowToCell rowIdx row) board


rowToCell : Int -> List Cell -> Html Msg
rowToCell rowIdx row =
    div [ class "w-full flex-1 flex flex-row" ] (List.indexedMap (\colIdx cell -> cellToHtml rowIdx colIdx cell) row)


cellToHtml : Int -> Int -> Cell -> Html Msg
cellToHtml rowIdx colIdx cell =
    case cell of
        On ->
            button [ class "flex-1 bg-gray-100 border-gray-200 border", onClick (ClickedCell rowIdx colIdx) ] []

        Off ->
            button [ class "flex-1 bg-gray-800 border-gray-100 border", onClick (ClickedCell rowIdx colIdx) ] []



---- Controls ----


gameControls : Model -> Html Msg
gameControls model =
    div [ class "flex-col p-8" ]
        [ div [ class "flex flex-row mb-10 pb-10 justify-around items-center border-b border-gray-400" ]
            [ div [ class "flex-1 flex justify-center items-center" ] [ solvedStatus model.board ]
            , div
                [ class "flex-1 flex justify-center items-center text-4xl font-black text-gray-800"
                , classList [ ( "text-green-500", solved model.board ) ]
                ]
                [ text <| String.fromInt model.moveCount ]
            ]
        , div [ class "flex flex-row justify-around items-center" ]
            [ div [ class "" ]
                [ InputHelpers.resetButton GetRandomBoard ]
            , div [ class "w-1/2" ]
                [ label [ class "block text-sm font-medium text-gray-700" ] [ text "Size" ]
                , div [ class "mt-1 relative rounded-md shadow-sm" ] [ numberInput 2 12 model.boardSize UpdateBoardSize ]
                ]
            ]
        ]


numberInput : Int -> Int -> Int -> (Int -> msg) -> Html msg
numberInput minValue maxValue n toMsg =
    let
        f v =
            case String.toInt v of
                Just i ->
                    toMsg i

                Nothing ->
                    toMsg 0
    in
    input
        [ type_ "number"
        , Attr.min <| String.fromInt minValue
        , Attr.max <| String.fromInt maxValue
        , value <| String.fromInt n
        , onInput f
        , class "block w-full p-1 pl-2 border-gray-500 border focus:outline-none focus:ring-blue-500 focus:border-blue-500 sm:text-sm rounded-md"
        ]
        []


solvedStatus : Board -> Html Msg
solvedStatus board =
    if solved board then
        div [ class "w-16 h-16 rounded-full bg-green-500 text-gray-100 flex items-center justify-center" ]
            [ FeatherIcons.unlock
                |> FeatherIcons.toHtml []
            ]

    else
        div [ class "w-16 h-16 rounded-full border-2 border-gray-800 bg-gray-100 text-gray-800 flex items-center justify-center" ]
            [ FeatherIcons.lock
                |> FeatherIcons.toHtml []
            ]
