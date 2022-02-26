module Header exposing (header)

import Html exposing (Html, a, div, footer, h1, h2, text)
import Html.Attributes exposing (alt, class, href)
import Route
import Svg exposing (Svg, svg)
import Svg.Attributes


header : Html msg
header =
    Html.header [ class "flex flex-row justify-between items-center border-b border-gray-200 py-2 px-4" ]
        [ h1 [ class "text-2xl" ]
            [ a [ Route.href Route.Home, class "h1 font-black" ] [ text "doodles.camp 🏕️" ]
            ]
        , socialLinks
        ]


socialLinks : Html msg
socialLinks =
    div [ class "text-gray-600 flex flex-row" ]
        [ homeLink
        , instagramLink
        , githubLink
        ]


githubLink : Html msg
githubLink =
    a
        [ class "w-12 h-12 flex justify-center items-center rounded-full p-2"
        , href "https://github.com/johnschoeman/doodles"
        , alt "Github"
        ]
        [ github
        ]


instagramLink : Html msg
instagramLink =
    a
        [ class "w-12 h-12 flex justify-center items-center rounded-full p-2"
        , href "https://www.instagram.com/johns1729/"
        , alt "Instagram"
        ]
        [ instagram
        ]


homeLink : Html msg
homeLink =
    a
        [ class "text-2xl w-12 h-12 flex justify-center items-center rounded-full p-2"
        , href "https://www.john.horse"
        , alt "Home"
        ]
        [ text "🤠"
        ]


github : Html msg
github =
    svgFeatherIcon "github"
        [ Svg.path [ Svg.Attributes.d "M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22" ] []
        ]


instagram : Html msg
instagram =
    svgFeatherIcon "instagram"
        [ Svg.rect
            [ Svg.Attributes.x "2"
            , Svg.Attributes.y "2"
            , Svg.Attributes.width "20"
            , Svg.Attributes.height "20"
            , Svg.Attributes.rx "5"
            , Svg.Attributes.ry "5"
            ]
            []
        , Svg.path
            [ Svg.Attributes.d "M16 11.37A4 4 0 1 1 12.63 8 4 4 0 0 1 16 11.37z"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "17.5"
            , Svg.Attributes.y1 "6.5"
            , Svg.Attributes.x2 "17.51"
            , Svg.Attributes.y2 "6.5"
            ]
            []
        ]


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ Svg.Attributes.class <| "feather feather-" ++ className
        , Svg.Attributes.fill "none"
        , Svg.Attributes.height "24"
        , Svg.Attributes.stroke "currentColor"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.width "24"
        ]
