module RecursionTest exposing (..)

import Expect
import Recursion exposing (..)
import Test exposing (..)


slowSum : Int -> Float
slowSum =
    runRecursion
        (\i ->
            case i of
                0 ->
                    base 0.0

                _ ->
                    recurse (i - 1) |> andThen (\f -> base <| f + toFloat i)
        )


fastSum : Int -> Float
fastSum i_ =
    runRecursion
        (\( i, acc ) ->
            case i of
                0 ->
                    base acc

                _ ->
                    recurse ( i - 1, acc + toFloat i )
        )
        ( i_, 0.0 )


type Tree a
    = Leaf a
    | Node (Tree a) (Tree a)


{-| Makes a tree of 2^n nodes
-}
makeDeepTree : (Int -> a) -> Int -> Tree a
makeDeepTree f =
    runRecursion
        (\i ->
            case i of
                0 ->
                    base <| Leaf (f 0)

                _ ->
                    recurse (i - 1) |> andThen (\tree -> base (Node tree tree))
        )


{-| Makes a tree of n nodes
-}
makeDeepLeftTree : (Int -> a) -> Int -> Tree a
makeDeepLeftTree f =
    runRecursion
        (\i ->
            case i of
                0 ->
                    base <| Leaf (f 0)

                _ ->
                    recurse (i - 1) |> andThen (\tree -> base (Node tree (Leaf <| f i)))
        )


mapTree : (a -> b) -> Tree a -> Tree b
mapTree f =
    runRecursion
        (\tree ->
            case tree of
                Leaf a ->
                    base <| Leaf (f a)

                Node l r ->
                    recurse l
                        |> andThen
                            (\l_ ->
                                recurse r
                                    |> andThen (\r_ -> base <| Node l_ r_)
                            )
        )


suite : Test
suite =
    describe "Recursion"
        [ test "slowSum doesn't stack overflow" <|
            \_ ->
                slowSum 100000 |> Expect.within (Expect.Absolute 0) ((100000.0 * 100001.0) / 2.0)
        , test "fastSum doesn't stack overflow" <|
            \_ ->
                fastSum 100000 |> Expect.within (Expect.Absolute 0) ((100000.0 * 100001.0) / 2.0)
        , test "makeDeepLeftTree doesn't stack overflow" <|
            \_ ->
                let
                    _ =
                        makeDeepTree identity 1000
                in
                Expect.pass
        , test "mapTree doens't stack overflow" <|
            \_ ->
                let
                    -- tree has 2^16 nodes
                    tree =
                        makeDeepTree identity 16

                    mapped =
                        mapTree (\x -> x) tree
                in
                Expect.equal tree mapped
        ]
