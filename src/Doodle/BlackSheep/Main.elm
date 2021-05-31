module Doodle.BlackSheep.Main exposing (Model, Msg, init, update, view)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Doodle.BlackSheep.GameScreen as Game
import Doodle.BlackSheep.Level as Level exposing (Level)
import Doodle.BlackSheep.LevelsScreen as Levels
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Session exposing (WithSession)
import Task


type alias Model =
    WithSession
        { viewport : Maybe Viewport
        , game : Game.Model
        , levels : List Level.Level
        , currentLevelId : Int
        }


init : Session.Model -> ( Model, Cmd Msg )
init session =
    let
        levels =
            Level.allLevels

        levelId =
            1
    in
    ( { session = session
      , viewport = Nothing
      , levels = levels
      , currentLevelId = 1
      , game = Game.init 1 levels
      }
    , Cmd.batch [ Task.perform GotViewport getViewport ]
    )


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



---- MODEL ----


initialLevels : List Level.Level
initialLevels =
    Level.allLevels


view : Model -> Html Msg
view model =
    Html.map (\gameMsg -> GotGameMsg gameMsg) (Game.view model.game)


goToGameCallback : Level -> Html.Attribute Msg
goToGameCallback level =
    onClick (GoToGame level)
