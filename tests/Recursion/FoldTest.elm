module Recursion.FoldTest exposing (suite)

import Expect
import Recursion exposing (..)
import Recursion.Fold exposing (..)
import Test exposing (..)


type RoseTree a
    = Leaf a
    | Node (List (RoseTree a))


countLeaves : RoseTree a -> Int
countLeaves =
    runRecursion <|
        \tree ->
            case tree of
                Leaf _ ->
                    base 1

                Node list ->
                    foldList (+) 0 list


type KeyedRoseTree a
    = KeyedLeaf a
    | KeyedNode (List ( String, KeyedRoseTree a ))


countKeyedLeaves : KeyedRoseTree a -> Int
countKeyedLeaves =
    runRecursion <|
        \tree ->
            case tree of
                KeyedLeaf _ ->
                    base 1

                KeyedNode list ->
                    foldMapList (\( _, node ) accum -> recurse node |> map ((+) accum)) 0 list


safetyTests : Test
safetyTests =
    describe "Safety Tests"
        [ test "countLeaves doesn't stack overflow" <|
            \_ ->
                let
                    tree =
                        List.range 1 100000 |> List.map Leaf |> Node
                in
                countLeaves tree |> Expect.equal 100000
        , test "countKeyedLeaves doesn't stack overflow" <|
            \_ ->
                let
                    tree =
                        List.range 1 100000 |> List.map (\i -> ( String.fromInt i, KeyedLeaf i )) |> KeyedNode
                in
                countKeyedLeaves tree |> Expect.equal 100000
        ]


suite : Test
suite =
    describe "Recursion.Fold"
        [ safetyTests ]
