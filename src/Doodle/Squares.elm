module Doodle.Squares exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class, style, type_, value)
import Session exposing (WithSession)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, rx, ry, viewBox, width, x, y)
import Task
import Time


type alias XPos = Int
type alias YPos = Int
type alias XDelta = Int
type alias YDelta = Int
type Color = Red | Green | Blue

 
type Square
    = Square Color XPos YPos XDelta YDelta


type alias Model =
    WithSession
        { viewport : Maybe Viewport
        , squares : List Square
        }


type Msg
    = GotViewport Viewport
    | Tick Time.Posix


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , viewport = Nothing
      , squares =
            [ Square Red 100 200 10 10
            , Square Green 200 100 1 2
            , Square Blue 50 100 2 3
            , Square Red 300 20 2 1
            ]
      }
    , Task.perform GotViewport getViewport
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        Tick now ->
            let
                time =
                    Debug.log <| Debug.toString now

                oldSquares =
                    model.squares
            in
            case model.viewport of
                Just viewport ->
                    ( { model | squares = tickSquares viewport oldSquares }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


tickSquares : Viewport -> List Square -> List Square
tickSquares viewport squares =
    List.map (tickSquare viewport) squares


tickSquare : Viewport -> Square -> Square
tickSquare viewport (Square color xPos yPos xDelta yDelta) =
    let
        { width, height } =
            viewport.viewport

        newXPos =
            modBy (ceiling width) (xPos + xDelta)

        newYPos =
            modBy (ceiling height) (yPos + yDelta)

    in
    Square (nextColor color) newXPos newYPos xDelta yDelta


nextColor : Color -> Color
nextColor color = 
  case color of
    Red -> Green
    Green -> Blue
    Blue -> Red

---- SUBSCRIPTIONS ----


subscriptions : Sub Msg
subscriptions =
    onAnimationFrame Tick



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.viewport of
        Just viewport ->
            div []
                [ art viewport model
                ]

        Nothing ->
            div [] [ text "loading..." ]


art : Viewport -> Model -> Html Msg
art viewport model =
    let
        { width, height } =
            viewport.viewport

        heightString =
            String.fromInt <| ceiling height

        widthString =
            String.fromInt <| ceiling width
    in
    div [ class "border bg-gray-100" ]
        [ svg [ viewBox <| String.join " " [ "0 0", widthString, heightString ] ]
            (List.map squareSvg model.squares)
        ]

showColor: Color -> String
showColor color =
  case color of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"

squareSvg : Square -> Html msg
squareSvg (Square color xPos yPos _ _) =
    rect
        [ x <| String.fromInt xPos
        , y <| String.fromInt yPos
        , fill <| showColor color
        , width "100"
        , height "100"
        , rx "2"
        , ry "2"
        ]
        []
