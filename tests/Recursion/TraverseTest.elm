module Recursion.TraverseTest exposing (suite)

import Expect
import Recursion exposing (..)
import Recursion.Traverse exposing (..)
import Test exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)

type RoseTree a
        = RTNode a (List (RoseTree a))

bigRoseTree : RoseTree Int
bigRoseTree = 
    List.range 1 10000
        |> List.map (\x -> RTNode x []) |> RTNode 0

sequenceListMap : (a -> b) -> RoseTree a -> RoseTree b
sequenceListMap f = 
    runRecursion <| 
        \(RTNode val list) -> 
            sequenceList list |> map (RTNode (f val))

sequenceListThenMap : (a -> b) -> RoseTree a -> RoseTree b
sequenceListThenMap f = 
    runRecursion <| 
        \(RTNode val list) -> 
            sequenceListThen list (RTNode (f val) >> base)


traverseListMap : (a -> b) -> RoseTree a -> RoseTree b
traverseListMap f = 
    runRecursion <| 
        \(RTNode val list) -> 
            traverseList recurse list |> map (RTNode (f val))

traverseListThenMap : (a -> b) -> RoseTree a -> RoseTree b
traverseListThenMap f = 
    runRecursion <| 
        \(RTNode val list) -> 
            traverseListThen recurse list (RTNode (f val) >> base)

type HashTrie a
    = HTLeaf a
    | HTNode (Dict String (HashTrie a))

bigHashTrie : HashTrie Int
bigHashTrie = 
    List.range 1 10000
        |> List.map (\x -> (String.fromInt x, HTLeaf x))
        |> Dict.fromList
        |> HTNode

sequenceDictMap : (a -> b) -> HashTrie a -> HashTrie b
sequenceDictMap f = 
    runRecursion <|
        \hash -> 
            case hash of
                HTLeaf a -> base (HTLeaf (f a))

                HTNode dict -> 
                    sequenceDict dict |> map HTNode

sequenceDictThenMap : (a -> b) -> HashTrie a -> HashTrie b
sequenceDictThenMap f = 
    runRecursion <|
        \hash -> 
            case hash of
                HTLeaf a -> base (HTLeaf (f a))

                HTNode dict -> 
                    sequenceDictThen dict (HTNode >> base)

traverseDictMap : (a -> b) -> HashTrie a -> HashTrie b
traverseDictMap f = 
    runRecursion <|
        \hash -> 
            case hash of
                HTLeaf a -> base (HTLeaf (f a))

                HTNode dict -> 
                    traverseDict (\_ v -> recurse v) dict |> map HTNode

traverseDictThenMap : (a -> b) -> HashTrie a -> HashTrie b
traverseDictThenMap f = 
    runRecursion <|
        \hash -> 
            case hash of
                HTLeaf a -> base (HTLeaf (f a))

                HTNode dict -> 
                    traverseDictThen (\_ v -> recurse v) dict (HTNode >> base)

type ArrayTree a
        = ATNode a (Array (ArrayTree a))

bigArrayTree : ArrayTree Int
bigArrayTree = 
    List.range 1 10000
        |> List.map (\x -> ATNode x (Array.fromList [])) 
        |> Array.fromList
        |> ATNode 0

sequenceArrayMap : (a -> b) -> ArrayTree a -> ArrayTree b
sequenceArrayMap f = 
    runRecursion <| 
        \(ATNode val array) -> 
            sequenceArray array |> map (ATNode (f val))

sequenceArrayThenMap : (a -> b) -> ArrayTree a -> ArrayTree b
sequenceArrayThenMap f = 
    runRecursion <| 
        \(ATNode val array) -> 
            sequenceArrayThen array (ATNode (f val) >> base)


traverseArrayMap : (a -> b) -> ArrayTree a -> ArrayTree b
traverseArrayMap f = 
    runRecursion <| 
        \(ATNode val array) -> 
            traverseArray recurse array |> map (ATNode (f val))

traverseArrayThenMap : (a -> b) -> ArrayTree a -> ArrayTree b
traverseArrayThenMap f = 
    runRecursion <| 
        \(ATNode val array) -> 
            traverseArrayThen recurse array (ATNode (f val) >> base)

stackSafetyTests : Test
stackSafetyTests = 
    describe "Stack Safety Tests"
        [ describe "List" 
            [ test "sequenceList doesn't overflow" <| 
                \_ ->  
                    let _ = sequenceListMap ((+) 1) bigRoseTree
                    in Expect.pass
            , test "sequenceListThen doesn't overflow" <| 
                \_ ->  
                    let _ = sequenceListThenMap ((+) 1) bigRoseTree
                    in Expect.pass
            , test "traverseList doesn't overflow" <| 
                \_ ->  
                    let _ = traverseListMap ((+) 1) bigRoseTree
                    in Expect.pass
            , test "traverseListThen doesn't overflow" <| 
                \_ ->  
                    let _ = traverseListThenMap ((+) 1) bigRoseTree
                    in Expect.pass
            ]
        , describe "Dict"
            [ test "sequenceDict doesn't overflow" <|
                \_ ->  
                    let _ = sequenceDictMap ((+) 1) bigHashTrie
                    in Expect.pass
            ,test "sequenceDictThen doesn't overflow" <|
                \_ ->  
                    let _ = sequenceDictThenMap ((+) 1) bigHashTrie
                    in Expect.pass
            , test "traverseDict doesn't overflow" <|
                \_ ->  
                    let _ = traverseDictMap ((+) 1) bigHashTrie
                    in Expect.pass
            , test "traverseDictThen doesn't overflow" <|
                \_ ->  
                    let _ = traverseDictThenMap ((+) 1) bigHashTrie
                    in Expect.pass
            ]
        , describe "Array"
            [ test "sequenceArray doesn't overflow" <| 
                \_ ->  
                    let _ = sequenceArrayMap ((+) 1) bigArrayTree
                    in Expect.pass
            , test "sequenceArrayThen doesn't overflow" <| 
                \_ ->  
                    let _ = sequenceArrayThenMap ((+) 1) bigArrayTree
                    in Expect.pass
            , test "traverseArray doesn't overflow" <| 
                \_ ->  
                    let _ = traverseArrayMap ((+) 1) bigArrayTree
                    in Expect.pass
            , test "traverseArrayThen doesn't overflow" <| 
                \_ ->  
                    let _ = traverseArrayThenMap ((+) 1) bigArrayTree
                    in Expect.pass
            ]
        ]

suite : Test
suite = 
    describe "Recursion.Traverse"
        [ stackSafetyTests ]
