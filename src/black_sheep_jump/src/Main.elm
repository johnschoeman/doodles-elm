module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import GameScreen as Game
import Html exposing (Html, a, button, div, h1, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Level as Level exposing (Level)
import LevelsScreen as Levels
import Task


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    ()



---- MODEL ----


initialLevels : List Level.Level
initialLevels =
    Level.allLevels


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        levels =
            Level.allLevels

        levelId =
            1
    in
    ( { viewport = Nothing
      , levels = levels
      , currentLevelId = 1
      , game = Game.init 1 levels
      }
    , Cmd.batch [ Task.perform GotViewport getViewport ]
    )


type alias Model =
    { viewport : Maybe Viewport
    , game : Game.Model
    , levels : List Level.Level
    , currentLevelId : Int
    }



---- UPDATE ----


type Msg
    = GotViewport Viewport
    | GoToGame Level
    | GotGameMsg Game.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        GotGameMsg subMsg ->
            let
                nextGame =
                    Game.update subMsg model.game

                nextLevels =
                    nextGame.levels
            in
            ( { model | game = nextGame, levels = nextLevels }
            , Cmd.none
            )

        GoToGame level ->
            ( { model | game = Game.init level.id model.levels }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        _ ->
            Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Black Sheep Jump"
    , body =
        [ div [ class "flex flex-col h-full w-full px-8 py-8" ]
            [ header
            , subView model
            ]
        ]
    }


subView : Model -> Html Msg
subView model =
    Html.map (\gameMsg -> GotGameMsg gameMsg) (Game.view model.game)


goToGameCallback : Level -> Html.Attribute Msg
goToGameCallback level =
    onClick (GoToGame level)


header : Html Msg
header =
    div [ class "flex items-baseline" ]
        [ h1 [ class "text-gray-800 text-3xl lg:text-4xl" ] [ text "Black Sheep Jump" ]
        , a [ class "lnk ml-12", href "https://www.doodles.camp/" ] [ text "back" ]
        ]
