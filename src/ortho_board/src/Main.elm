module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random


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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        _ ->
            Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "doodles.camp"
    , body =
        [ subView model
        ]
    }


type alias Model =
    { board : Board }


type alias Board =
    List Row


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
            Cell v ->
                Cell (v + 1)

    else
        cell


type alias Row =
    List Cell


type Cell
    = Cell Int


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { board = initialBoard }, generateRandomBoard )


initialBoard : Board
initialBoard =
    [ [] ]


generateRandomBoard : Cmd Msg
generateRandomBoard =
    Random.generate RandomizeBoard <| randomBoard 4 12


randomBoard : Int -> Int -> Random.Generator Board
randomBoard width height =
    Random.list height (Random.list width randomCell)


randomCell : Random.Generator Cell
randomCell =
    Random.map Cell (Random.int 0 12)


type Msg
    = RandomizeBoard Board
    | ClickedCell Int Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell row col ->
            let
                nextBoard =
                    updateBoard row col model.board
            in
            ( { model | board = nextBoard }, Cmd.none )

        RandomizeBoard board ->
            ( { model | board = board }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subView : Model -> Html Msg
subView model =
    div [ class "" ]
        [ viewBoard model.board
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    div [ class "flex flex-col w-[25vw] h-[75vw] max-h-full max-w-[25vh] space-y-0" ] (List.indexedMap viewRow board)


viewRow : Int -> Row -> Html Msg
viewRow rowIdx row =
    div [ class "flex-1 flex flex-row space-x-0" ] (List.indexedMap (\colIdx -> viewCell rowIdx colIdx) row)


viewCell : Int -> Int -> Cell -> Html Msg
viewCell rowIdx colIdx c =
    let
        colorStyle =
            determineColor c
    in
    div [ class <| "w-full h-full " ++ colorStyle, onClick (ClickedCell rowIdx colIdx) ] []


determineColor : Cell -> String
determineColor cell =
    case cell of
        Cell v ->
            if modBy 2 v == 0 then
                "bg-yellow-400 hover:bg-yellow-500"

            else if modBy 11 v == 0 then
                "bg-white hover:bg-gray-100"

            else
                "bg-blue-400 hover:bg-blue-500"
