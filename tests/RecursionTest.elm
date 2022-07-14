module RecursionTest exposing (..)

import Expect exposing (Expectation)
import Recursion as Recursion exposing (..)
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


mapTree : (a -> b) -> Tree a -> Tree b
mapTree f =
    runRecursion
        (\tree ->
            case tree of
                Leaf a ->
                    base (Leaf <| f a)

                Node leftA rightA ->
                    recurse leftA |> andThen (\leftB -> recurse rightA |> andThen (\rightB -> base <| Node leftB rightB))
        )


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


type RoseTree a
    = RoseLeaf a
    | RoseNode (List (RoseTree a))
    | KeyedRoseNode (List ( String, RoseTree a ))


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
                        mapTree (\x -> x + 1) tree
                in
                Expect.equal mapped mapped
        , test "mapTree doens't stack overflow 2" <|
            \_ ->
                let
                    _ =
                        makeDeepLeftTree identity 100000 |> mapTree (\x -> x + 1)
                in
                Expect.pass
        ]
