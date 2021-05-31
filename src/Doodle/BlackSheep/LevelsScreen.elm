module Doodle.BlackSheep.LevelsScreen exposing (Model, Msg, update, view)

import Dict
import Doodle.BlackSheep.Level as Level exposing (Level, allLevels, toString)
import Html exposing (Html, a, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Model =
    {}



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



---- VIEW ----


view : (Level -> Html.Attribute msg) -> List Level -> Html msg
view navigateCallback levels =
    div []
        [ ul [] (List.map (levelListItem navigateCallback) levels)
        ]


levelListItem : (Level -> Html.Attribute msg) -> Level -> Html msg
levelListItem navigateCallback level =
    li [ class "flex w-full py-2 px-1 justify-between border-b border-gray-800" ]
        [ button [ navigateCallback level, class "text-gray-800 font-semibold" ] [ text (Level.toString level) ]
        , levelCompletedIcon level
        ]


levelCompletedIcon : Level -> Html msg
levelCompletedIcon level =
    if level.completed then
        img [ src "stars-24px.svg" ] []

    else
        text ""
