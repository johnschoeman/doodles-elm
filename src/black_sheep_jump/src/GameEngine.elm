module GameEngine exposing
    ( State(..)
    , anyValidMovesForNode
    , gameState
    , moveIsValid
    , update
    )

import Board as Board exposing (Board, Node(..), Status(..))


type State
    = Won
    | Lost
    | InProgress


update : Node -> Node -> Board -> Board
update fromNode toNode board =
    let
        fromStatus =
            Board.getDataAtNode board fromNode

        maybeNeighborNode =
            Board.getNeighborNode fromNode toNode
    in
    case maybeNeighborNode of
        Just neighborNode ->
            if moveIsValid board fromNode toNode then
                case fromStatus of
                    BlackDot ->
                        setCell neighborNode Empty board
                            |> setCell toNode BlackDot
                            |> setCell fromNode Empty

                    Dot ->
                        setCell neighborNode Empty board
                            |> setCell toNode Dot
                            |> setCell fromNode Empty

                    Empty ->
                        board

            else
                board

        Nothing ->
            board


setCell : Node -> Status -> Board -> Board
setCell node status board =
    Board.updateBoardByNode node status board


gameState : Board -> State
gameState board =
    let
        count =
            Board.dotCount board
    in
    if count == 0 then
        Won

    else if anyValidMoves board then
        InProgress

    else
        Lost


anyValidMovesForNode : Board -> Node -> Node -> Bool -> Bool
anyValidMovesForNode board fromNode toNode acc =
    case acc of
        True ->
            True

        False ->
            moveIsValid board fromNode toNode


moveIsValid : Board -> Node -> Node -> Bool
moveIsValid board fromNode toNode =
    let
        maybeNeighborNode =
            Board.getNeighborNode fromNode toNode
    in
    case maybeNeighborNode of
        Nothing ->
            False

        Just neighborNode ->
            let
                fromStatus =
                    Board.getDataAtNode board fromNode

                neighborStatus =
                    Board.getDataAtNode board neighborNode

                toStatus =
                    Board.getDataAtNode board toNode
            in
            (fromStatus == Dot || fromStatus == BlackDot)
                && (neighborStatus == Dot && neighborStatus /= BlackDot)
                && (toStatus == Empty)


anyValidMoves : Board -> Bool
anyValidMoves board =
    List.foldr (anyValidMovesOnBoard board) False Board.allNodes


anyValidMovesOnBoard : Board -> Node -> Bool -> Bool
anyValidMovesOnBoard board fromNode acc =
    case acc of
        True ->
            True

        False ->
            hasValidMove board fromNode


hasValidMove : Board -> Node -> Bool
hasValidMove board fromNode =
    List.foldl (anyValidMovesForNode board fromNode) False Board.allNodes
