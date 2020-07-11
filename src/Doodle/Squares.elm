module Doodle.Squares exposing (Model, Msg, subscriptions, init, update, view)

import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, text, div)
import Html.Attributes as Attr exposing (class, style, type_, value)
import Session exposing (WithSession)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, rx, ry, viewBox, width, x, y)
import Time
import Task


type  Square = Square Int Int

type alias Model =
    WithSession
        { viewport : Maybe Viewport,
         square: Square
        }


type Msg
    = GotViewport Viewport
    | Tick Time.Posix


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , viewport = Nothing
      , square = Square 100 200
      }, Task.perform GotViewport getViewport
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )
        Tick now ->
          let
              time = Debug.log <| Debug.toString now
              oldSquare = model.square
          in
          ( { model | square = tickSquare oldSquare}, Cmd.none)


tickSquare : Square -> Square
tickSquare (Square xPos yPos) =
  let
      xDelta = 1
      yDelta = 1
  in
  Square (xPos + xDelta) (yPos + yDelta)


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
                [
                 art viewport model
                ]

        Nothing ->
            div [] [ text "loading..." ]


art : Viewport -> Model -> Html Msg
art viewport model =
   let
       { width, height } = viewport.viewport
       heightString = String.fromInt <| ceiling height
       widthString = String.fromInt <| ceiling width

    in
    div [ class "border bg-gray-100" ]
        [ svg [ viewBox <| String.join " " [ "0 0", heightString, widthString ] ]
            [
              squareSvg model.square
            ]
        ]



squareSvg : Square -> Html msg
squareSvg (Square xPos yPos) =
    svg
        [ width "400"
        , height "400"
        , viewBox "0 0 400 400"
        ]
        [ rect
            [ x <| String.fromInt xPos
            , y <| String.fromInt yPos
            , fill "blue"
            , width "100"
            , height "100"
            , rx "2"
            , ry "2"
            ]
            []
        ]
