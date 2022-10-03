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


foldMapListCount : ListTree a -> Int
foldMapListCount =
    runRecursion <|
        \(ListNode _ list) ->
            foldMapList (\x accum -> recurse x |> map ((+) accum)) 1 list


foldDictCount : DictTree a -> Int
foldDictCount =
    runRecursion <|
        \(DictNode _ dict) ->
            foldDict (\_ x accum -> x + accum) 1 dict


foldMapDictCount : DictTree a -> Int
foldMapDictCount =
    runRecursion <|
        \(DictNode _ dict) ->
            foldMapDict (\_ x accum -> recurse x |> map ((+) accum)) 1 dict


foldArrayCount : ArrayTree a -> Int
foldArrayCount =
    runRecursion <|
        \(ArrayNode _ array) ->
            foldArray (+) 1 array


foldMapArrayCount : ArrayTree a -> Int
foldMapArrayCount =
    runRecursion <|
        \(ArrayNode _ array) ->
            foldMapArray (\x accum -> recurse x |> map ((+) accum)) 1 array


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
            , test "foldMapList doesn't overflow" <|
                \_ ->
                    foldMapListCount hugeListTree |> Expect.equal expectedHugeSize
            ]
        , describe "Dict"
            [ test "foldDict doesn't overflow" <|
                \_ ->
                    foldDictCount hugeDictTree |> Expect.equal expectedHugeSize
            , test "foldMapDict doesn't overflow" <|
                \_ ->
                    foldMapDictCount hugeDictTree |> Expect.equal expectedHugeSize
            ]
        , describe "Array"
            [ test "foldArray doesn't overflow" <|
                \_ ->
                    foldArrayCount hugeArrayTree |> Expect.equal expectedHugeSize
            , test "foldMapArray doesn't overflow" <|
                \_ ->
                    foldMapArrayCount hugeArrayTree |> Expect.equal expectedHugeSize
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
             , test "foldMapList" <| \_ -> Expect.equal 3 (foldMapListCount initListTree)
             ]
            )
        , describe "Dict"
            (let
                initDictTree =
                    DictNode 2 (Dict.fromList [ ( "1", DictNode 1 Dict.empty ), ( "3", DictNode 3 Dict.empty ) ])
             in
             [ test "foldDict" <| \_ -> Expect.equal 3 (foldDictCount initDictTree)
             , test "foldMapDict" <| \_ -> Expect.equal 3 (foldMapDictCount initDictTree)
             ]
            )
        , describe "Array"
            (let
                initArrayTree =
                    ArrayNode 2 (Array.fromList [ ArrayNode 1 Array.empty, ArrayNode 3 Array.empty ])
             in
             [ test "sequenceArray" <| \_ -> Expect.equal 3 (foldArrayCount initArrayTree)
             , test "traverseArray" <| \_ -> Expect.equal 3 (foldMapArrayCount initArrayTree)
             ]
            )
        ]


suite : Test
suite =
    describe "Recursion.Fold"
        [ safetyTests
        , correctnessTests
        ]
