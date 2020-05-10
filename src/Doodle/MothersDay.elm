module Doodle.MothersDay exposing (Model, init, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Session exposing (WithSession)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, stroke, strokeWidth, viewBox, x, y)


type alias Model =
    WithSession {}


init : Session.Model -> Model
init session =
    { session = session }


view : Html msg
view =
    let
        vb =
            viewBox "0 0 300 300"
    in
    div [ class "border bg-gray-100" ]
        [ svg [ vb ]
            [ path
                [ d heartSvg
                , stroke "pink"
                , strokeWidth "1"
                , fill "pink"
                ]
                []
            , svg [ vb ]
                [ Svg.text_
                    [ stroke "gray", fill "pink", x "20", y "120" ]
                    [ text "Happy Mother's Day Mom!" ]
                ]
            , svg [ vb ]
                [ Svg.text_
                    [ stroke "gray", fill "pink", x "20", y "150" ]
                    [ text "I love you <3" ]
                ]
            ]
        ]


heartSvg : String
heartSvg =
    String.join " "
        [ "M 10,30"
        , "A 20,20 0,0,1 50,30"
        , "A 20,20 0,0,1 90,30"
        , "Q 90,60 50,90"
        , "Q 10,60 10,30 z"
        ]
