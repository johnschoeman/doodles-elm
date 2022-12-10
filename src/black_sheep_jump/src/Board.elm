module Board exposing
    ( Board
    , Destination
    , Neighbor
    , Node(..)
    , Start
    , Status(..)
    , allBoards
    , allNodes
    , board1
    , boardDictionary
    , byId
    , dotCount
    , getDataAtNode
    , getNeighborNode
    , updateBoardByNode
    )

import Dict


type alias A =
    Node


type Node
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M


allNodes : List Node
allNodes =
    [ A, B, C, D, E, F, G, H, I, J, K, L, M ]


type Status
    = Dot
    | BlackDot
    | Empty


type alias Start =
    String


type alias Neighbor =
    String


type alias Destination =
    String


type alias Board =
    { a : Status
    , b : Status
    , c : Status
    , d : Status
    , e : Status
    , f : Status
    , g : Status
    , h : Status
    , i : Status
    , j : Status
    , k : Status
    , l : Status
    , m : Status
    }


nodeToString : Node -> String
nodeToString node =
    case node of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        D ->
            "D"

        E ->
            "E"

        F ->
            "F"

        G ->
            "G"

        H ->
            "H"

        I ->
            "I"

        J ->
            "J"

        K ->
            "K"

        L ->
            "L"

        M ->
            "M"


statusToString : Status -> String
statusToString status =
    case status of
        Empty ->
            "E"

        Dot ->
            "D"

        BlackDot ->
            "B"



--- if a node has a neighbor, then the jump is considered possible by the game


getNeighborNode : Node -> Node -> Maybe Node
getNeighborNode fromNode toNode =
    case fromNode of
        A ->
            case toNode of
                C ->
                    Just B

                G ->
                    Just D

                K ->
                    Just F

                _ ->
                    Nothing

        B ->
            case toNode of
                F ->
                    Just D

                L ->
                    Just G

                H ->
                    Just E

                _ ->
                    Nothing

        C ->
            case toNode of
                A ->
                    Just B

                G ->
                    Just E

                M ->
                    Just H

                _ ->
                    Nothing

        D ->
            case toNode of
                J ->
                    Just G

                _ ->
                    Nothing

        E ->
            case toNode of
                I ->
                    Just G

                _ ->
                    Nothing

        F ->
            case toNode of
                B ->
                    Just D

                H ->
                    Just G

                L ->
                    Just I

                _ ->
                    Nothing

        G ->
            case toNode of
                A ->
                    Just D

                C ->
                    Just E

                K ->
                    Just I

                M ->
                    Just J

                _ ->
                    Nothing

        H ->
            case toNode of
                B ->
                    Just E

                F ->
                    Just G

                L ->
                    Just J

                _ ->
                    Nothing

        I ->
            case toNode of
                E ->
                    Just G

                _ ->
                    Nothing

        J ->
            case toNode of
                D ->
                    Just G

                _ ->
                    Nothing

        K ->
            case toNode of
                A ->
                    Just F

                G ->
                    Just I

                M ->
                    Just L

                _ ->
                    Nothing

        L ->
            case toNode of
                F ->
                    Just I

                B ->
                    Just G

                H ->
                    Just J

                _ ->
                    Nothing

        M ->
            case toNode of
                K ->
                    Just L

                G ->
                    Just J

                C ->
                    Just H

                _ ->
                    Nothing


updateBoardByNode : Node -> Status -> Board -> Board
updateBoardByNode node status board =
    case node of
        A ->
            { board | a = status }

        B ->
            { board | b = status }

        C ->
            { board | c = status }

        D ->
            { board | d = status }

        E ->
            { board | e = status }

        F ->
            { board | f = status }

        G ->
            { board | g = status }

        H ->
            { board | h = status }

        I ->
            { board | i = status }

        J ->
            { board | j = status }

        K ->
            { board | k = status }

        L ->
            { board | l = status }

        M ->
            { board | m = status }


getDataAtNode : Board -> Node -> Status
getDataAtNode board node =
    case node of
        A ->
            board.a

        B ->
            board.b

        C ->
            board.c

        D ->
            board.d

        E ->
            board.e

        F ->
            board.f

        G ->
            board.g

        H ->
            board.h

        I ->
            board.i

        J ->
            board.j

        K ->
            board.k

        L ->
            board.l

        M ->
            board.m


dotCount : Board -> Int
dotCount board =
    List.foldl (addDots board) 0 allNodes


addDots : Board -> Node -> Int -> Int
addDots board node acc =
    hasDot (getDataAtNode board node) + acc


hasDot : Status -> Int
hasDot status =
    if status == Dot then
        1

    else
        0


allBoards : List Board
allBoards =
    [ board1
    , board2
    , board3
    , board4
    , board5
    , board6
    , board7
    , board8
    , board9
    , board10
    , board11
    , board12
    , board13
    , board14
    , board15
    , board16
    , board17
    , board18
    , board19
    , board20
    , board21
    , board22
    , board23
    , board24
    , board25
    , board26
    , board27
    , board28
    , board29
    , board30
    , board31
    , board32
    , board33
    , board34
    , board35
    , board36
    , board37
    , board38
    , board39
    , board40
    , board41
    , board42
    , board43
    , board44
    , board45
    ]


byId : Int -> Board
byId id =
    let
        maybeBoard =
            List.drop id allBoards
                |> List.head
    in
    case maybeBoard of
        Just board ->
            board

        Nothing ->
            board1


boardDictionary : Dict.Dict Int Board
boardDictionary =
    List.indexedMap (\idx board -> ( idx, board )) allBoards
        |> Dict.fromList


emptyBoard : Board
emptyBoard =
    { a = Empty
    , b = Empty
    , c = Empty
    , d = Empty
    , e = Empty
    , f = Empty
    , g = Empty
    , h = Empty
    , i = Empty
    , j = Empty
    , k = Empty
    , l = Empty
    , m = Empty
    }


board1 : Board
board1 =
    { emptyBoard
        | g = Dot
        , i = BlackDot
        , j = Dot
        , m = Dot
    }


board2 : Board
board2 =
    { emptyBoard
        | a = Dot
        , b = BlackDot
        , d = Dot
        , i = Dot
        , k = Dot
    }


board3 : Board
board3 =
    { emptyBoard
        | b = BlackDot
        , g = Dot
        , i = Dot
        , l = Dot
        , m = Dot
    }


board4 : Board
board4 =
    { emptyBoard
        | f = Dot
        , g = BlackDot
        , i = Dot
        , j = Dot
        , k = Dot
    }


board5 : Board
board5 =
    { emptyBoard
        | a = BlackDot
        , e = Dot
        , g = Dot
        , h = Dot
        , l = Dot
    }


board6 : Board
board6 =
    { emptyBoard
        | b = Dot
        , g = BlackDot
        , i = Dot
        , j = Dot
        , l = Dot
        , m = Dot
    }


board7 : Board
board7 =
    { emptyBoard
        | e = Dot
        , f = Dot
        , g = BlackDot
        , i = Dot
        , j = Dot
        , k = Dot
    }


board8 : Board
board8 =
    { emptyBoard
        | a = Dot
        , b = Dot
        , f = Dot
        , i = Dot
        , k = BlackDot
        , l = Dot
    }


board9 : Board
board9 =
    { emptyBoard
        | b = Dot
        , d = Dot
        , e = Dot
        , g = BlackDot
        , i = Dot
        , j = Dot
        , l = Dot
    }


board10 : Board
board10 =
    { emptyBoard
        | b = Dot
        , d = Dot
        , e = BlackDot
        , i = Dot
        , j = Dot
        , l = Dot
        , m = Dot
    }


board11 : Board
board11 =
    { emptyBoard
        | a = Dot
        , b = Dot
        , d = Dot
        , e = Dot
        , f = BlackDot
        , g = Dot
        , m = Dot
    }


board12 : Board
board12 =
    { emptyBoard
        | b = Dot
        , d = Dot
        , e = BlackDot
        , i = Dot
        , j = Dot
        , k = Dot
    }


board13 : Board
board13 =
    { emptyBoard
        | b = Dot
        , e = Dot
        , f = BlackDot
        , h = Dot
        , i = Dot
        , l = Dot
        , m = Dot
    }


board14 : Board
board14 =
    { emptyBoard
        | b = Dot
        , d = BlackDot
        , e = Dot
        , f = Dot
        , g = Dot
        , i = Dot
        , j = Dot
        , m = Dot
    }


board15 : Board
board15 =
    { emptyBoard
        | b = Dot
        , c = Dot
        , d = Dot
        , e = Dot
        , h = Dot
        , i = Dot
        , j = Dot
        , k = BlackDot
        , l = Dot
    }


board16 : Board
board16 =
    { emptyBoard
        | a = Dot
        , b = BlackDot
        , c = Dot
        , d = Dot
        , e = Dot
        , f = Dot
        , h = Dot
    }


board17 : Board
board17 =
    { emptyBoard
        | b = Dot
        , c = Dot
        , e = Dot
        , f = Dot
        , g = Dot
        , h = Dot
        , i = Dot
        , k = BlackDot
        , m = Dot
    }


board18 : Board
board18 =
    { emptyBoard
        | a = Dot
        , b = Dot
        , d = Dot
        , e = Dot
        , f = Dot
        , g = Dot
        , h = Dot
        , i = Dot
        , j = BlackDot
        , m = Dot
    }


board19 : Board
board19 =
    { emptyBoard
        | a = BlackDot
        , e = Dot
        , f = Dot
        , i = Dot
        , k = Dot
        , l = Dot
    }


board20 : Board
board20 =
    { emptyBoard
        | b = Dot
        , d = Dot
        , e = Dot
        , g = Dot
        , h = Dot
        , i = BlackDot
        , j = Dot
        , l = Dot
    }


board21 : Board
board21 =
    { emptyBoard
        | d = BlackDot
        , e = Dot
        , f = Dot
        , i = Dot
        , j = Dot
        , k = Dot
        , l = Dot
    }


board22 : Board
board22 =
    { emptyBoard
        | d = Dot
        , e = Dot
        , f = Dot
        , h = Dot
        , i = Dot
        , j = BlackDot
        , l = Dot
        , m = Dot
    }


board23 : Board
board23 =
    { emptyBoard
        | b = Dot
        , c = BlackDot
        , d = Dot
        , e = Dot
        , h = Dot
        , i = Dot
        , j = Dot
        , l = Dot
        , m = Dot
    }


board24 : Board
board24 =
    { emptyBoard
        | a = Dot
        , b = Dot
        , d = Dot
        , e = BlackDot
        , f = Dot
        , g = Dot
        , i = Dot
        , j = Dot
        , m = Dot
    }


board25 : Board
board25 =
    { emptyBoard
        | b = BlackDot
        , c = Dot
        , d = Dot
        , f = Dot
        , h = Dot
        , i = Dot
        , j = Dot
        , k = Dot
        , l = Dot
    }


board26 : Board
board26 =
    { emptyBoard
        | b = Dot
        , d = Dot
        , e = Dot
        , f = Dot
        , g = Dot
        , i = Dot
        , j = BlackDot
        , l = Dot
    }


board27 : Board
board27 =
    { emptyBoard
        | a = Dot
        , d = Dot
        , f = Dot
        , g = Dot
        , i = BlackDot
        , j = Dot
        , l = Dot
        , m = Dot
    }


board28 : Board
board28 =
    { emptyBoard
        | a = Dot
        , c = Dot
        , d = Dot
        , e = Dot
        , g = Dot
        , i = Dot
        , j = Dot
        , k = Dot
        , l = BlackDot
        , m = Dot
    }


board29 : Board
board29 =
    { emptyBoard
        | a = Dot
        , b = Dot
        , d = Dot
        , e = Dot
        , f = Dot
        , h = Dot
        , i = Dot
        , j = BlackDot
        , l = Dot
    }


board30 : Board
board30 =
    { emptyBoard
        | d = Dot
        , e = BlackDot
        , f = Dot
        , g = Dot
        , i = Dot
        , j = Dot
        , k = Dot
        , l = Dot
    }


board31 : Board
board31 =
    { emptyBoard
        | b = Dot
        , c = Dot
        , d = Dot
        , e = Dot
        , f = Dot
        , g = Dot
        , i = Dot
        , j = Dot
        , l = Dot
        , m = BlackDot
    }


board32 : Board
board32 =
    { emptyBoard
        | a = Dot
        , b = Dot
        , c = BlackDot
        , d = Dot
        , e = Dot
        , i = Dot
        , j = Dot
        , k = Dot
        , l = Dot
        , m = Dot
    }


board33 : Board
board33 =
    { emptyBoard
        | a = Dot
        , d = Dot
        , f = Dot
        , h = Dot
        , i = Dot
        , j = Dot
        , k = Dot
        , l = BlackDot
    }


board34 : Board
board34 =
    { emptyBoard
        | d = Dot
        , e = Dot
        , g = BlackDot
        , i = Dot
        , j = Dot
        , k = Dot
        , l = Dot
        , m = Dot
    }


board35 : Board
board35 =
    { emptyBoard
        | b = Dot
        , c = Dot
        , d = Dot
        , e = Dot
        , f = Dot
        , h = Dot
        , i = Dot
        , j = Dot
        , k = BlackDot
        , m = Dot
    }


board36 : Board
board36 =
    { emptyBoard
        | a = Dot
        , d = Dot
        , e = BlackDot
        , f = Dot
        , i = Dot
        , j = Dot
        , k = Dot
        , l = Dot
        , m = Dot
    }


board37 : Board
board37 =
    { emptyBoard
        | a = Dot
        , c = Dot
        , d = Dot
        , e = Dot
        , f = Dot
        , j = Dot
        , l = Dot
        , m = BlackDot
    }


board38 : Board
board38 =
    { emptyBoard
        | a = Dot
        , b = BlackDot
        , c = Dot
        , d = Dot
        , f = Dot
        , g = Dot
        , h = Dot
        , i = Dot
        , j = Dot
        , m = Dot
    }


board39 : Board
board39 =
    { emptyBoard
        | a = BlackDot
        , b = Dot
        , c = Dot
        , d = Dot
        , e = Dot
        , f = Dot
        , g = Dot
        , h = Dot
        , i = Dot
        , j = Dot
        , l = Dot
    }


board40 : Board
board40 =
    { emptyBoard
        | b = Dot
        , c = Dot
        , e = Dot
        , f = Dot
        , g = BlackDot
        , i = Dot
        , j = Dot
        , l = Dot
        , m = Dot
    }


board41 : Board
board41 =
    { emptyBoard
        | a = Dot
        , d = Dot
        , e = Dot
        , f = Dot
        , g = BlackDot
        , h = Dot
        , i = Dot
        , j = Dot
        , k = Dot
        , m = Dot
    }


board42 : Board
board42 =
    { emptyBoard
        | a = Dot
        , d = Dot
        , e = Dot
        , f = Dot
        , g = BlackDot
        , j = Dot
        , l = Dot
        , m = Dot
    }


board43 : Board
board43 =
    { emptyBoard
        | a = Dot
        , b = Dot
        , d = Dot
        , e = Dot
        , g = BlackDot
        , i = Dot
        , l = Dot
        , m = Dot
    }


board44 : Board
board44 =
    { emptyBoard
        | a = Dot
        , b = Dot
        , c = Dot
        , d = Dot
        , e = Dot
        , f = Dot
        , g = BlackDot
        , i = Dot
        , j = Dot
        , k = Dot
    }


board45 : Board
board45 =
    { emptyBoard
        | a = BlackDot
        , b = Dot
        , c = Dot
        , e = Dot
        , f = Dot
        , h = Dot
        , i = Dot
        , j = Dot
        , k = Dot
        , l = Dot
        , m = Dot
    }
