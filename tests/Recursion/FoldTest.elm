module Recursion.FoldTest exposing (suite)

import Array
import Dict
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


expectedHugeSize : Int
expectedHugeSize =
    hugeSize + 1


safetyTests : Test
safetyTests =
    describe "Safety Tests"
        [ describe "List"
            [ test "foldList doesn't overflow" <|
                \_ ->
                    foldListCount hugeListTree |> Expect.equal expectedHugeSize
            , test "foldListThen doesn't overflow" <|
                \_ ->
                    foldListThenCount hugeListTree |> Expect.equal expectedHugeSize
            , test "foldMapList doesn't overflow" <|
                \_ ->
                    foldMapListCount hugeListTree |> Expect.equal expectedHugeSize
            , test "foldMapListThen doesn't overflow" <|
                \_ ->
                    foldMapListThenCount hugeListTree |> Expect.equal expectedHugeSize
            ]
        , describe "Dict"
            [ test "foldDict doesn't overflow" <|
                \_ ->
                    foldDictCount hugeDictTree |> Expect.equal expectedHugeSize
            , test "foldDictThen doesn't overflow" <|
                \_ ->
                    foldDictThenCount hugeDictTree |> Expect.equal expectedHugeSize
            , test "foldMapDict doesn't overflow" <|
                \_ ->
                    foldMapDictCount hugeDictTree |> Expect.equal expectedHugeSize
            , test "foldMapDictThen doesn't overflow" <|
                \_ ->
                    foldMapDictThenCount hugeDictTree |> Expect.equal expectedHugeSize
            ]
        , describe "Array"
            [ test "foldArray doesn't overflow" <|
                \_ ->
                    foldArrayCount hugeArrayTree |> Expect.equal expectedHugeSize
            , test "foldArrayThen doesn't overflow" <|
                \_ ->
                    foldArrayThenCount hugeArrayTree |> Expect.equal expectedHugeSize
            , test "foldMapArray doesn't overflow" <|
                \_ ->
                    foldMapArrayCount hugeArrayTree |> Expect.equal expectedHugeSize
            , test "foldMapArrayThen doesn't overflow" <|
                \_ ->
                    foldMapArrayThenCount hugeArrayTree |> Expect.equal expectedHugeSize
            ]
        ]


correctnessTests : Test
correctnessTests =
    describe "Correctness tests"
        [ describe "List"
            (let
                initListTree =
                    ListNode 2 [ ListNode 1 [], ListNode 3 [] ]
             in
             [ test "foldList" <| \_ -> Expect.equal 3 (foldListCount initListTree)
             , test "foldListThen" <| \_ -> Expect.equal 3 (foldListThenCount initListTree)
             , test "foldMapList" <| \_ -> Expect.equal 3 (foldMapListCount initListTree)
             , test "foldMapListThen" <| \_ -> Expect.equal 3 (foldMapListThenCount initListTree)
             ]
            )
        , describe "Dict"
            (let
                initDictTree =
                    DictNode 2 (Dict.fromList [ ( "1", DictNode 1 Dict.empty ), ( "3", DictNode 3 Dict.empty ) ])
             in
             [ test "foldDict" <| \_ -> Expect.equal 3 (foldDictCount initDictTree)
             , test "foldDictThen" <| \_ -> Expect.equal 3 (foldDictThenCount initDictTree)
             , test "foldMapDict" <| \_ -> Expect.equal 3 (foldMapDictCount initDictTree)
             , test "foldMapDictThen" <| \_ -> Expect.equal 3 (foldMapDictThenCount initDictTree)
             ]
            )
        , describe "Array"
            (let
                initArrayTree =
                    ArrayNode 2 (Array.fromList [ ArrayNode 1 Array.empty, ArrayNode 3 Array.empty ])
             in
             [ test "sequenceArray" <| \_ -> Expect.equal 3 (foldArrayCount initArrayTree)
             , test "sequenceArrayThen" <| \_ -> Expect.equal 3 (foldArrayThenCount initArrayTree)
             , test "traverseArray" <| \_ -> Expect.equal 3 (foldMapArrayCount initArrayTree)
             , test "traverseArrayThen" <| \_ -> Expect.equal 3 (foldMapArrayThenCount initArrayTree)
             ]
            )
        ]


suite : Test
suite =
    describe "Recursion.Fold"
        [ safetyTests
        , correctnessTests
        ]
