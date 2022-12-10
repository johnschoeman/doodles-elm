module Level exposing (Level, allLevels, getLevel, level1, toString, updateLevels)

import Board as Board exposing (Board, allBoards)
import Dict exposing (Dict)
import GameEngine as GameEngine


type alias Level =
    { id : Int
    , board : Board
    , completed : Bool
    }


level1 : Level
level1 =
    { id = 1
    , board = Board.board1
    , completed = False
    }


allLevels : List Level
allLevels =
    List.indexedMap (\idx board -> { id = idx, board = board, completed = False }) allBoards


toString : Level -> String
toString level =
    "Level " ++ String.fromInt level.id


getLevel : Int -> List Level -> Maybe Level
getLevel id levels =
    List.drop id levels |> List.head


updateLevels : GameEngine.State -> Int -> List Level -> List Level
updateLevels gameState levelId levels =
    if gameState == GameEngine.Won then
        List.indexedMap (\idx level -> updateLevel idx levelId level) levels

    else
        levels


updateLevel : Int -> Int -> Level -> Level
updateLevel idx gameLevelId level =
    if idx == level.id && idx == gameLevelId then
        { level | completed = True }

    else
        level
