module Recursion.TraverseTest exposing (suite)

import Array
import Dict
import Expect exposing (Expectation)
import Recursion exposing (..)
import Recursion.TestTypes exposing (..)
import Recursion.Traverse exposing (..)
import Test exposing (..)


sequenceListMap : (a -> b) -> ListTree a -> ListTree b
sequenceListMap f =
    runRecursion <|
        \(ListNode val list) ->
            sequenceList list |> map (ListNode (f val))


traverseListMap : (a -> b) -> ListTree a -> ListTree b
traverseListMap f =
    runRecursion <|
        \(ListNode val list) ->
            traverseList recurse list |> map (ListNode (f val))


sequenceDictMap : (a -> b) -> DictTree a -> DictTree b
sequenceDictMap f =
    runRecursion <|
        \(DictNode val dict) ->
            sequenceDict dict |> map (DictNode (f val))


traverseDictMap : (a -> b) -> DictTree a -> DictTree b
traverseDictMap f =
    runRecursion <|
        \(DictNode val dict) ->
            traverseDict (\_ v -> recurse v) dict |> map (DictNode (f val))


sequenceArrayMap : (a -> b) -> ArrayTree a -> ArrayTree b
sequenceArrayMap f =
    runRecursion <|
        \(ArrayNode val array) ->
            sequenceArray array |> map (ArrayNode (f val))


traverseArrayMap : (a -> b) -> ArrayTree a -> ArrayTree b
traverseArrayMap f =
    runRecursion <|
        \(ArrayNode val array) ->
            traverseArray recurse array |> map (ArrayNode (f val))


sequenceMaybeMap : (a -> b) -> MaybeTree a -> MaybeTree b
sequenceMaybeMap f =
    runRecursion <|
        \(MaybeNode val array) ->
            sequenceMaybe array |> map (MaybeNode (f val))


traverseMaybeMap : (a -> b) -> MaybeTree a -> MaybeTree b
traverseMaybeMap f =
    runRecursion <|
        \(MaybeNode val array) ->
            traverseMaybe recurse array |> map (MaybeNode (f val))


sequenceResultMap : (a -> b) -> ResultTree a -> ResultTree b
sequenceResultMap f =
    runRecursion <|
        \(ResultNode val array) ->
            sequenceResult array |> map (ResultNode (f val))


traverseResultMap : (a -> b) -> ResultTree a -> ResultTree b
traverseResultMap f =
    runRecursion <|
        \(ResultNode val array) ->
            traverseResult recurse array |> map (ResultNode (f val))


listTreeExists : ListTree a -> Expectation
listTreeExists _ =
    Expect.pass


dictTreeExists : DictTree a -> Expectation
dictTreeExists _ =
    Expect.pass


arrayTreeExists : ArrayTree a -> Expectation
arrayTreeExists _ =
    Expect.pass


stackSafetyTests : Test
stackSafetyTests =
    describe "Stack Safety Tests"
        [ describe "List"
            [ test "sequenceList doesn't overflow" <| \_ -> sequenceListMap ((+) 1) hugeListTree |> listTreeExists
            , test "traverseList doesn't overflow" <| \_ -> traverseListMap ((+) 1) hugeListTree |> listTreeExists
            ]
        , describe "Dict"
            [ test "sequenceDict doesn't overflow" <| \_ -> sequenceDictMap ((+) 1) hugeDictTree |> dictTreeExists
            , test "traverseDict doesn't overflow" <| \_ -> traverseDictMap ((+) 1) hugeDictTree |> dictTreeExists
            ]
        , describe "Array"
            [ test "sequenceArray doesn't overflow" <| \_ -> sequenceArrayMap ((+) 1) hugeArrayTree |> arrayTreeExists
            , test "traverseArray doesn't overflow" <| \_ -> traverseArrayMap ((+) 1) hugeArrayTree |> arrayTreeExists
            ]
        ]


correctnessTests : Test
correctnessTests =
    describe "Correctness tests"
        [ describe "List"
            (let
                initListTree =
                    ListNode 2 [ ListNode 1 [], ListNode 3 [] ]

                expectedListTree =
                    ListNode 3 [ ListNode 2 [], ListNode 4 [] ]
             in
             [ test "sequenceList" <| \_ -> Expect.equal expectedListTree (sequenceListMap ((+) 1) initListTree)
             , test "traverseList" <| \_ -> Expect.equal expectedListTree (traverseListMap ((+) 1) initListTree)
             ]
            )
        , describe "Dict"
            (let
                initDictTree =
                    DictNode 2 (Dict.fromList [ ( "1", DictNode 1 Dict.empty ), ( "3", DictNode 3 Dict.empty ) ])

                expectedDictTree =
                    DictNode 3 (Dict.fromList [ ( "1", DictNode 2 Dict.empty ), ( "3", DictNode 4 Dict.empty ) ])
             in
             [ test "sequenceDict" <| \_ -> Expect.equal expectedDictTree (sequenceDictMap ((+) 1) initDictTree)
             , test "traverseDict" <| \_ -> Expect.equal expectedDictTree (traverseDictMap ((+) 1) initDictTree)
             ]
            )
        , describe "Array"
            (let
                initArrayTree =
                    ArrayNode 2 (Array.fromList [ ArrayNode 1 Array.empty, ArrayNode 3 Array.empty ])

                expectedArrayTree =
                    ArrayNode 3 (Array.fromList [ ArrayNode 2 Array.empty, ArrayNode 4 Array.empty ])
             in
             [ test "sequenceArray" <| \_ -> Expect.equal expectedArrayTree (sequenceArrayMap ((+) 1) initArrayTree)
             , test "traverseArray" <| \_ -> Expect.equal expectedArrayTree (traverseArrayMap ((+) 1) initArrayTree)
             ]
            )
        , describe "Maybe"
            (let
                initMaybeTree =
                    MaybeNode 1 (Just <| MaybeNode 2 (Just <| MaybeNode 3 Nothing))

                expectedMaybeTree =
                    MaybeNode 2 (Just <| MaybeNode 3 (Just <| MaybeNode 4 Nothing))
             in
             [ test "sequenceMaybe" <| \_ -> Expect.equal expectedMaybeTree (sequenceMaybeMap ((+) 1) initMaybeTree)
             , test "traverseMaybe" <| \_ -> Expect.equal expectedMaybeTree (traverseMaybeMap ((+) 1) initMaybeTree)
             ]
            )
        , describe "Result"
            (let
                initResultTree =
                    ResultNode 1 (Ok <| ResultNode 2 (Ok <| ResultNode 3 (Err "a")))

                expectedResultTree =
                    ResultNode 2 (Ok <| ResultNode 3 (Ok <| ResultNode 4 (Err "a")))
             in
             [ test "sequenceResult" <| \_ -> Expect.equal expectedResultTree (sequenceResultMap ((+) 1) initResultTree)
             , test "traverseResult" <| \_ -> Expect.equal expectedResultTree (traverseResultMap ((+) 1) initResultTree)
             ]
            )
        ]


suite : Test
suite =
    describe "Recursion.Traverse"
        [ stackSafetyTests
        , correctnessTests
        ]
