module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Navigation as Nav
import Doodle.BlackSheep.Main as BlackSheep
import Doodle.Dots as Dots
import Doodle.LockPuzzle as LockPuzzle
import Doodle.ModularTimesTable.Main as ModularTimesTable
import Doodle.MothersDay as MothersDay
import Doodle.Recaman as Recaman
import Doodle.Squares as Squares
import Home
import Html exposing (Html, a, div, footer, h1, h2, text)
import Html.Attributes exposing (alt, class, href)
import Route
import Session
import Svg exposing (Svg, svg)
import Svg.Attributes
import Url


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }


type alias Flags =
    ()



---- MODEL ----


type Model
    = NotFound Session.Model
    | Loading Session.Model
    | Home Home.Model
    | Dots Dots.Model
    | Squares Squares.Model
    | Recaman Recaman.Model
    | MothersDay MothersDay.Model
    | LockPuzzle LockPuzzle.Model
    | BlackSheep BlackSheep.Model
    | ModularTimesTable ModularTimesTable.Model


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        session =
            Session.initial key
    in
    changeRouteTo (Route.fromUrl url)
        (Loading session)



---- UPDATE ----


type Msg
    = ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | GotViewport Viewport
    | HandleHomeMsg Home.Msg
    | HandleDotsMsg Dots.Msg
    | HandleSquaresMsg Squares.Msg
    | HandleRecamanMsg Recaman.Msg
    | HandleLockPuzzleMsg LockPuzzle.Msg
    | HandleBlackSheepMsg BlackSheep.Msg
    | HandleModularTimesTableMsg ModularTimesTable.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( HandleHomeMsg subMsg, Home model_ ) ->
            let
                ( newModel, newMsg ) =
                    Home.update model_ subMsg
            in
            ( Home newModel
            , Cmd.map HandleHomeMsg newMsg
            )

        ( HandleDotsMsg subMsg, Dots model_ ) ->
            let
                ( newModel, newMsg ) =
                    Dots.update subMsg model_
            in
            ( Dots newModel
            , Cmd.map HandleDotsMsg newMsg
            )

        ( HandleSquaresMsg subMsg, Squares model_ ) ->
            let
                ( newModel, newMsg ) =
                    Squares.update subMsg model_
            in
            ( Squares newModel
            , Cmd.map HandleSquaresMsg newMsg
            )

        ( HandleRecamanMsg subMsg, Recaman model_ ) ->
            let
                ( newModel, newMsg ) =
                    Recaman.update subMsg model_
            in
            ( Recaman newModel
            , Cmd.map HandleRecamanMsg newMsg
            )

        ( HandleLockPuzzleMsg subMsg, LockPuzzle model_ ) ->
            let
                ( newModel, newMsg ) =
                    LockPuzzle.update subMsg model_
            in
            ( LockPuzzle newModel
            , Cmd.map HandleLockPuzzleMsg newMsg
            )

        ( HandleBlackSheepMsg subMsg, BlackSheep model_ ) ->
            let
                ( newModel, newMsg ) =
                    BlackSheep.update subMsg model_
            in
            ( BlackSheep newModel
            , Cmd.map HandleBlackSheepMsg newMsg
            )

        ( HandleModularTimesTableMsg subMsg, ModularTimesTable model_ ) ->
            let
                ( newModel, newMsg ) =
                    ModularTimesTable.update subMsg model_
            in
            ( ModularTimesTable newModel
            , Cmd.map HandleModularTimesTableMsg newMsg
            )

        ( _, _ ) ->
            ( model, Cmd.none )


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWith Home HandleHomeMsg

        Just Route.Dots ->
            Dots.init session
                |> updateWith Dots HandleDotsMsg

        Just Route.Squares ->
            Squares.init session
                |> updateWith Squares HandleSquaresMsg

        Just Route.Recaman ->
            Recaman.init session
                |> updateWith Recaman HandleRecamanMsg

        Just Route.MothersDay ->
            ( MothersDay <| MothersDay.init session, Cmd.none )

        Just Route.LockPuzzle ->
            LockPuzzle.init session
                |> updateWith LockPuzzle HandleLockPuzzleMsg

        Just Route.BlackSheep ->
            BlackSheep.init session
                |> updateWith BlackSheep HandleBlackSheepMsg

        Just Route.ModularTimesTable ->
            ModularTimesTable.init session
                |> updateWith ModularTimesTable HandleModularTimesTableMsg


toSession : Model -> Session.Model
toSession model =
    case model of
        NotFound v ->
            v

        Loading v ->
            v

        Home { session } ->
            session

        Dots { session } ->
            session

        Squares { session } ->
            session

        Recaman { session } ->
            session

        MothersDay { session } ->
            session

        LockPuzzle { session } ->
            session

        BlackSheep { session } ->
            session

        ModularTimesTable { session } ->
            session


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home homeModel ->
            Sub.map HandleHomeMsg <| Home.subscriptions homeModel

        Squares squareModel ->
            Sub.map HandleSquaresMsg <| Squares.subscriptions

        _ ->
            Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Doodles"
    , body =
        [ header
        , pageContent model
        , pageFooter
        ]
    }


header : Html Msg
header =
    Html.header [ class "flex flex-row justify-between items-center border-b border-gray-200 py-2 px-4" ]
        [ h1 [ class "text-2xl" ]
            [ a [ Route.href Route.Home, class "h1 font-black" ] [ text "doodles.camp 🏕️" ]
            ]
        , socialLinks
        ]


withSubHeader : String -> Html Msg -> Html Msg
withSubHeader subheaderText html =
    div [ class "space-y-4" ]
        [ h1 [ class "h1 pl-4 pt-4" ] [ text subheaderText ]
        , html
        ]


pageContent : Model -> Html Msg
pageContent model =
    case model of
        Home m ->
            Html.map HandleHomeMsg <| Home.view m

        Dots m ->
            withSubHeader
                "dots"
                (Html.map HandleDotsMsg <| Dots.view m)

        Squares m ->
            withSubHeader
                "squares"
                (Html.map HandleSquaresMsg <| Squares.view m)

        Recaman m ->
            withSubHeader
                "recaman"
                (Html.map HandleRecamanMsg <| Recaman.view m)

        MothersDay m ->
            MothersDay.view

        LockPuzzle m ->
            withSubHeader
                "lock puzzle"
                (Html.map HandleLockPuzzleMsg <| LockPuzzle.view m)

        BlackSheep m ->
            withSubHeader
                "black sheep"
                (Html.map HandleBlackSheepMsg <| BlackSheep.view m)

        ModularTimesTable m ->
            withSubHeader
                "modular times table"
                (Html.map HandleModularTimesTableMsg <| ModularTimesTable.view m)

        NotFound _ ->
            h2 [] [ text "Page not found" ]

        Loading _ ->
            text ""


pageFooter : Html a
pageFooter =
    footer []
        [ div [] [ text "" ]
        ]


socialLinks : Html Msg
socialLinks =
    div [ class "text-gray-600 flex flex-row" ]
        [ homeLink
        , instagramLink
        , githubLink
        ]


githubLink : Html Msg
githubLink =
    a
        [ class "w-12 h-12 flex justify-center items-center rounded-full p-2"
        , href "https://github.com/johnschoeman/doodles"
        , alt "Github"
        ]
        [ github
        ]


instagramLink : Html Msg
instagramLink =
    a
        [ class "w-12 h-12 flex justify-center items-center rounded-full p-2"
        , href "https://www.instagram.com/johns1729/"
        , alt "Instagram"
        ]
        [ instagram
        ]


homeLink : Html Msg
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


home : Html msg
home =
    svgFeatherIcon "home"
        [ Svg.path
            [ Svg.Attributes.d "M3 9l9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"
            ]
            []
        , Svg.polyline
            [ Svg.Attributes.points "9 22 9 12 15 12 15 22"
            ]
            []
        ]
