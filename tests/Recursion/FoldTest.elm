module Recursion.FoldTest exposing (suite)

import Expect
import Recursion exposing (..)
import Recursion.Fold exposing (..)
import Recursion.TestTypes exposing (..)
import Test exposing (..)


foldListCount : ListTree a -> Int
foldListCount =
    runRecursion <|
        \(ListNode _ list) ->
            foldList (+) 1 list


foldListThenCount : ListTree a -> Int
foldListThenCount =
    runRecursion <|
        \(ListNode _ list) ->
            foldListThen (+) 1 list base


foldMapListCount : ListTree a -> Int
foldMapListCount =
    runRecursion <|
        \(ListNode _ list) ->
            foldMapList (\x accum -> recurseThen x ((+) accum >> base)) 1 list


foldMapListThenCount : ListTree a -> Int
foldMapListThenCount =
    runRecursion <|
        \(ListNode _ list) ->
            foldMapListThen (\x accum -> recurseThen x ((+) accum >> base)) 1 list base


foldDictCount : DictTree a -> Int
foldDictCount =
    runRecursion <|
        \(DictNode _ dict) ->
            foldDict (\_ x accum -> x + accum) 1 dict


foldDictThenCount : DictTree a -> Int
foldDictThenCount =
    runRecursion <|
        \(DictNode _ dict) ->
            foldDictThen (\_ x accum -> x + accum) 1 dict base


foldMapDictCount : DictTree a -> Int
foldMapDictCount =
    runRecursion <|
        \(DictNode _ dict) ->
            foldMapDict (\_ x accum -> recurseThen x ((+) accum >> base)) 1 dict


foldMapDictThenCount : DictTree a -> Int
foldMapDictThenCount =
    runRecursion <|
        \(DictNode _ dict) ->
            foldMapDictThen (\_ x accum -> recurseThen x ((+) accum >> base)) 1 dict base


foldArrayCount : ArrayTree a -> Int
foldArrayCount =
    runRecursion <|
        \(ArrayNode _ array) ->
            foldArray (+) 1 array


foldArrayThenCount : ArrayTree a -> Int
foldArrayThenCount =
    runRecursion <|
        \(ArrayNode _ array) ->
            foldArrayThen (+) 1 array base


foldMapArrayCount : ArrayTree a -> Int
foldMapArrayCount =
    runRecursion <|
        \(ArrayNode _ array) ->
            foldMapArray (\x accum -> recurseThen x ((+) accum >> base)) 1 array


foldMapArrayThenCount : ArrayTree a -> Int
foldMapArrayThenCount =
    runRecursion <|
        \(ArrayNode _ array) ->
            foldMapArrayThen (\x accum -> recurseThen x ((+) accum >> base)) 1 array base


safetyTests : Test
safetyTests =
    describe "Safety Tests"
        [ describe "List"
            [ test "foldList doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldListCount hugeListTree
                    in
                    Expect.pass
            , test "foldListThen doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldListThenCount hugeListTree
                    in
                    Expect.pass
            , test "foldMapList doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldMapListCount hugeListTree
                    in
                    Expect.pass
            , test "foldMapListThen doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldMapListThenCount hugeListTree
                    in
                    Expect.pass
            ]
        , describe "Dict"
            [ test "foldDict doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldDictCount hugeDictTree
                    in
                    Expect.pass
            , test "foldDictThen doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldDictThenCount hugeDictTree
                    in
                    Expect.pass
            , test "foldMapDict doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldMapDictCount hugeDictTree
                    in
                    Expect.pass
            , test "foldMapDictThen doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldMapDictThenCount hugeDictTree
                    in
                    Expect.pass
            ]
        , describe "Array"
            [ test "foldArray doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldArrayCount hugeArrayTree
                    in
                    Expect.pass
            , test "foldArrayThen doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldArrayThenCount hugeArrayTree
                    in
                    Expect.pass
            , test "foldMapArray doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldMapArrayCount hugeArrayTree
                    in
                    Expect.pass
            , test "foldMapArrayThen doesn't overflow" <|
                \_ ->
                    let
                        _ =
                            foldMapArrayThenCount hugeArrayTree
                    in
                    Expect.pass
            ]
        ]


suite : Test
suite =
    describe "Recursion.Fold"
        [ safetyTests ]
