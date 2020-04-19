module InputHelpers exposing (..)

import Html exposing (Html, button, div, img, option, select, text)
import Html.Attributes exposing (class, selected, src, value)
import Html.Events exposing (onClick, onInput)
import Svg exposing (use)
import Svg.Attributes exposing (x, y)


squareButton : msg -> String -> Html msg
squareButton toMsg t =
    let
        styles =
            "border-black border border-b-4 border-r-4 w-16 h-16"
    in
    button [ class styles, onClick toMsg ] [ text t ]


type Option a
    = Option String String a


dropDown : (String -> msg) -> a -> List (Option a) -> Html msg
dropDown toMsg s options =
    let
        containerStyle =
            "w-full md:w-1/3 px-3 mb-6 md:mb-0"

        selectStyle =
            "block appearance-none w-full bg-gray-200 border border-gray-200 text-gray-700 py-3 px-4 pr-8 rounded leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
    in
    div [ class containerStyle ]
        [ div [ class "relative" ]
            [ select [ class selectStyle, onInput toMsg ]
                (List.map (dropDownOption s) options)
            ]
        ]


dropDownOption : a -> Option a -> Html msg
dropDownOption s (Option v t item) =
    let
        style =
            "border-2"
    in
    option [ class style, value v, selected (item == s) ] [ text t ]
