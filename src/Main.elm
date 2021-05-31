module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Navigation as Nav
import Doodle.BlackSheep.Main as BlackSheep
import Doodle.Dots as Dots
import Doodle.LockPuzzle as LockPuzzle
import Doodle.MothersDay as MothersDay
import Doodle.Recaman as Recaman
import Doodle.Squares as Squares
import Home
import Html exposing (Html, a, div, footer, h1, h2, text)
import Html.Attributes exposing (class)
import Route
import Session
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
    Html.header [ class "border-b border-gray-400 py-4" ]
        [ h1 [ class "ml-4 text-2xl" ]
            [ a [ Route.href Route.Home, class "lnk" ] [ text "doodles.camp" ]
            ]
        ]


pageContent : Model -> Html Msg
pageContent model =
    case model of
        Home m ->
            Html.map HandleHomeMsg <| Home.view m

        Dots m ->
            Html.map HandleDotsMsg <| Dots.view m

        Squares m ->
            Html.map HandleSquaresMsg <| Squares.view m

        Recaman m ->
            Html.map HandleRecamanMsg <| Recaman.view m

        MothersDay m ->
            MothersDay.view

        LockPuzzle m ->
            Html.map HandleLockPuzzleMsg <| LockPuzzle.view m

        BlackSheep m ->
            Html.map HandleBlackSheepMsg <| BlackSheep.view m

        NotFound _ ->
            h2 [] [ text "Page not found" ]

        Loading _ ->
            text ""


pageFooter : Html a
pageFooter =
    footer []
        [ div [] [ text "" ]
        ]
