module Nat exposing (..)


type Nat
    = Z
    | S (Thunk Nat)


type alias Thunk a =
    () -> a


force : Thunk a -> a
force thunk =
    thunk ()


fromInt : Int -> Nat
fromInt n =
    if n <= 0 then
        Z

    else
        S (\_ -> fromInt (n - 1))


toInt : Nat -> Int
toInt =
    let
        foo acc n =
            case n of
                Z ->
                    acc

                S n_ ->
                    foo (1 + acc) (force n_)
    in
    foo 0



-- foldl : (b -> b) -> b -> Nat -> b
-- foldl f acc n =
--     case n of
--         Z ->
--             acc
--
--         S n_ ->
--             foldl f (f acc) n_
--
--
-- strNat : Nat -> String
-- strNat n =
--     foldl ((++) "S") "Z" n
--
--
-- toInt : Nat -> Int
-- toInt =
--     foldl ((+) 1) 0
--
--
--
-- -- plus : Nat -> Nat -> Nat
-- -- plus x y =
-- --     let
-- --         foo acc n =
-- --             case n of
-- --                 Z ->
-- --                     acc
-- --
-- --                 S n_ ->
-- --                     foo (S acc) n_
-- --     in
-- --     foo x y
--
--
-- plus : Nat -> Nat -> Nat
-- plus =
--     foldl S
--
--
-- eqNat : Nat -> Nat -> Bool
-- eqNat =
--     let
--         foo x y =
--             case ( x, y ) of
--                 ( Z, Z ) ->
--                     True
--
--                 ( S x_, S y_ ) ->
--                     foo x_ y_
--
--                 _ ->
--                     False
--     in
--     foo
