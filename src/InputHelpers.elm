module InputHelpers exposing (..)

import FeatherIcons
import Html exposing (Html, button, div, img, label, option, select, text)
import Html.Attributes exposing (class, for, id, selected, src, value)
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


dropDown : (String -> msg) -> a -> String -> List (Option a) -> Html msg
dropDown toMsg selectedItem labelText options =
    div []
        [ label [ for labelText, class "block text-sm font-medium text-gray-700" ] [ text labelText ]
        , select
            [ onInput toMsg
            , id labelText
            , class "mt-1 block w-full pl-3 pr-10 py-2 text-base border border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
            ]
            (List.map (dropDownOption selectedItem) options)
        ]


dropDownOption : a -> Option a -> Html msg
dropDownOption s (Option v t item) =
    option [ value v, selected (item == s) ] [ text t ]



---- Buttons ----


addButton : msg -> Html msg
addButton onClickMsg =
    button
        [ onClick onClickMsg
        , class "inline-flex items-center px-6 py-3 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
        ]
        [ addIcon ]


subtractButton : msg -> Html msg
subtractButton onClickMsg =
    button
        [ onClick onClickMsg
        , class "inline-flex items-center px-6 py-3 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
        ]
        [ subtractIcon ]


resetButton : msg -> Html msg
resetButton onClickMsg =
    button
        [ onClick onClickMsg
        , class "inline-flex items-center px-6 py-3 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
        ]
        [ resetIcon ]


resetIcon : Html msg
resetIcon =
    FeatherIcons.refreshCw |> FeatherIcons.toHtml []


addIcon : Html msg
addIcon =
    FeatherIcons.plus |> FeatherIcons.toHtml []


subtractIcon : Html msg
subtractIcon =
    FeatherIcons.minus |> FeatherIcons.toHtml []
