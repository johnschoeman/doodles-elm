module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Navigation as Nav
import Doodles.Dots as Dots
import Doodles.Squares as Squares
import Home
import Html exposing (Html, a, div, footer, h1, h2, text)
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

        _ ->
            Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Doodles"
    , body =
        [ h1 [] [ a [ Route.href Route.Home ] [ text "Doodles" ] ]
        , pageContent model
        , pageFooter
        ]
    }


pageContent : Model -> Html Msg
pageContent model =
    case model of
        Home m ->
            Html.map HandleHomeMsg <| Home.view m

        Dots m ->
            Html.map HandleDotsMsg <| Dots.view m

        Squares m ->
            Html.map HandleSquaresMsg <| Squares.view m

        NotFound _ ->
            h2 [] [ text "Page not found" ]

        Loading _ ->
            text ""


pageFooter : Html a
pageFooter =
    footer []
        [ div [] [ text "" ]
        ]
