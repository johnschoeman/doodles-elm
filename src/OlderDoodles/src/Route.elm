module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , replaceUrl
    )

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Home
    | Dots
    | Squares
    | Recaman
    | MothersDay
    | LockPuzzle
    | BlackSheep
    | ModularTimesTable


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Dots (s "doodles" </> s "dots")
        , Parser.map Squares (s "doodles" </> s "squares")
        , Parser.map Recaman (s "doodles" </> s "recaman")
        , Parser.map MothersDay (s "doodles" </> s "mom")
        , Parser.map LockPuzzle (s "doodles" </> s "lock-puzzle")
        , Parser.map BlackSheep (s "doodles" </> s "black-sheep")
        , Parser.map ModularTimesTable (s "doodles" </> s "modular-times-table")
        ]


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Dots ->
                    [ "doodles", "dots" ]

                Squares ->
                    [ "doodles", "squares" ]

                Recaman ->
                    [ "doodles", "recaman" ]

                MothersDay ->
                    [ "doodles", "mom" ]

                LockPuzzle ->
                    [ "doodles", "lock-puzzle" ]

                BlackSheep ->
                    [ "doodles", "black-sheep" ]

                ModularTimesTable ->
                    [ "doodles", "modular-times-table" ]
    in
    "#/" ++ String.join "/" pieces
