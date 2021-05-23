module Doodle.LockPuzzle exposing (Model, Msg, init, update, view)

import Browser.Dom exposing (Viewport, getViewport)
import FeatherIcons
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra exposing (updateAt)
import Session exposing (WithSession)
import Task


type alias Model =
    WithSession
        { viewport : Maybe Viewport
        , board : Board
        }


type alias Board =
    List (List Cell)


type Cell
    = On
    | Off


initialBoard : Board
initialBoard =
    buildBoard 4


buildBoard : Int -> Board
buildBoard size =
    List.repeat size (buildRow size)


buildRow : Int -> List Cell
buildRow size =
    List.repeat size Off


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , viewport = Nothing
      , board = initialBoard
      }
    , Task.perform GotViewport getViewport
    )


type Msg
    = GotViewport Viewport
    | ClickedCell Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        ClickedCell row col ->
            let
                nextBoard =
                    updateBoard row col model.board
            in
            ( { model | board = nextBoard }, Cmd.none )


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



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.viewport of
        Just viewport ->
            div [ class "w-full p-8 flex flex-row" ]
                [ div [] (boardRows model.board)
                , div [ class "w-full flex justify-center items-center" ] [ solvedStatus model.board ]
                ]

        Nothing ->
            div [] [ text "loading..." ]


boardRows : Board -> List (Html Msg)
boardRows board =
    List.indexedMap (\rowIdx row -> rowToCell rowIdx row) board


rowToCell : Int -> List Cell -> Html Msg
rowToCell rowIdx row =
    div [ class "flex flex-row" ] (List.indexedMap (\colIdx cell -> cellToHtml rowIdx colIdx cell) row)


cellToHtml : Int -> Int -> Cell -> Html Msg
cellToHtml rowIdx colIdx cell =
    case cell of
        On ->
            div [ class "w-48 h-48 bg-gray-100 border-gray-800", onClick (ClickedCell rowIdx colIdx) ] []

        Off ->
            div [ class "w-48 h-48 bg-gray-800 border-gray-100 border", onClick (ClickedCell rowIdx colIdx) ] []


solvedStatus : Board -> Html Msg
solvedStatus board =
    if solved board then
        div [ class "w-16 h-16 rounded-full bg-green-500 text-gray-100 flex items-center justify-center" ]
            [ FeatherIcons.unlock
                |> FeatherIcons.toHtml []
            ]

    else
        div [ class "w-16 h-16 rounded-full bg-gray-800 text-gray-100 flex items-center justify-center" ]
            [ FeatherIcons.lock
                |> FeatherIcons.toHtml []
            ]
