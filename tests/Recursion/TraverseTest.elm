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


sequenceListThenMap : (a -> b) -> ListTree a -> ListTree b
sequenceListThenMap f =
    runRecursion <|
        \(ListNode val list) ->
            sequenceListThen list (ListNode (f val) >> base)


traverseListMap : (a -> b) -> ListTree a -> ListTree b
traverseListMap f =
    runRecursion <|
        \(ListNode val list) ->
            traverseList recurse list |> map (ListNode (f val))


traverseListThenMap : (a -> b) -> ListTree a -> ListTree b
traverseListThenMap f =
    runRecursion <|
        \(ListNode val list) ->
            traverseListThen recurse list (ListNode (f val) >> base)


sequenceDictMap : (a -> b) -> DictTree a -> DictTree b
sequenceDictMap f =
    runRecursion <|
        \(DictNode val dict) ->
            sequenceDict dict |> map (DictNode (f val))


sequenceDictThenMap : (a -> b) -> DictTree a -> DictTree b
sequenceDictThenMap f =
    runRecursion <|
        \(DictNode val dict) ->
            sequenceDictThen dict (DictNode (f val) >> base)


traverseDictMap : (a -> b) -> DictTree a -> DictTree b
traverseDictMap f =
    runRecursion <|
        \(DictNode val dict) ->
            traverseDict (\_ v -> recurse v) dict |> map (DictNode (f val))


traverseDictThenMap : (a -> b) -> DictTree a -> DictTree b
traverseDictThenMap f =
    runRecursion <|
        \(DictNode val dict) ->
            traverseDictThen (\_ v -> recurse v) dict (DictNode (f val) >> base)


sequenceArrayMap : (a -> b) -> ArrayTree a -> ArrayTree b
sequenceArrayMap f =
    runRecursion <|
        \(ArrayNode val array) ->
            sequenceArray array |> map (ArrayNode (f val))


sequenceArrayThenMap : (a -> b) -> ArrayTree a -> ArrayTree b
sequenceArrayThenMap f =
    runRecursion <|
        \(ArrayNode val array) ->
            sequenceArrayThen array (ArrayNode (f val) >> base)


traverseArrayMap : (a -> b) -> ArrayTree a -> ArrayTree b
traverseArrayMap f =
    runRecursion <|
        \(ArrayNode val array) ->
            traverseArray recurse array |> map (ArrayNode (f val))


traverseArrayThenMap : (a -> b) -> ArrayTree a -> ArrayTree b
traverseArrayThenMap f =
    runRecursion <|
        \(ArrayNode val array) ->
            traverseArrayThen recurse array (ArrayNode (f val) >> base)


sequenceMaybeMap : (a -> b) -> MaybeTree a -> MaybeTree b
sequenceMaybeMap f =
    runRecursion <|
        \(MaybeNode val array) ->
            sequenceMaybe array |> map (MaybeNode (f val))


sequenceMaybeThenMap : (a -> b) -> MaybeTree a -> MaybeTree b
sequenceMaybeThenMap f =
    runRecursion <|
        \(MaybeNode val array) ->
            sequenceMaybeThen array (MaybeNode (f val) >> base)


traverseMaybeMap : (a -> b) -> MaybeTree a -> MaybeTree b
traverseMaybeMap f =
    runRecursion <|
        \(MaybeNode val array) ->
            traverseMaybe recurse array |> map (MaybeNode (f val))


traverseMaybeThenMap : (a -> b) -> MaybeTree a -> MaybeTree b
traverseMaybeThenMap f =
    runRecursion <|
        \(MaybeNode val array) ->
            traverseMaybeThen recurse array (MaybeNode (f val) >> base)


sequenceResultMap : (a -> b) -> ResultTree a -> ResultTree b
sequenceResultMap f =
    runRecursion <|
        \(ResultNode val array) ->
            sequenceResult array |> map (ResultNode (f val))


sequenceResultThenMap : (a -> b) -> ResultTree a -> ResultTree b
sequenceResultThenMap f =
    runRecursion <|
        \(ResultNode val array) ->
            sequenceResultThen array (ResultNode (f val) >> base)


traverseResultMap : (a -> b) -> ResultTree a -> ResultTree b
traverseResultMap f =
    runRecursion <|
        \(ResultNode val array) ->
            traverseResult recurse array |> map (ResultNode (f val))


traverseResultThenMap : (a -> b) -> ResultTree a -> ResultTree b
traverseResultThenMap f =
    runRecursion <|
        \(ResultNode val array) ->
            traverseResultThen recurse array (ResultNode (f val) >> base)


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
            , test "sequenceListThen doesn't overflow" <| \_ -> sequenceListThenMap ((+) 1) hugeListTree |> listTreeExists
            , test "traverseList doesn't overflow" <| \_ -> traverseListMap ((+) 1) hugeListTree |> listTreeExists
            , test "traverseListThen doesn't overflow" <| \_ -> traverseListThenMap ((+) 1) hugeListTree |> listTreeExists
            ]
        , describe "Dict"
            [ test "sequenceDict doesn't overflow" <| \_ -> sequenceDictMap ((+) 1) hugeDictTree |> dictTreeExists
            , test "sequenceDictThen doesn't overflow" <| \_ -> sequenceDictThenMap ((+) 1) hugeDictTree |> dictTreeExists
            , test "traverseDict doesn't overflow" <| \_ -> traverseDictMap ((+) 1) hugeDictTree |> dictTreeExists
            , test "traverseDictThen doesn't overflow" <| \_ -> traverseDictThenMap ((+) 1) hugeDictTree |> dictTreeExists
            ]
        , describe "Array"
            [ test "sequenceArray doesn't overflow" <| \_ -> sequenceArrayMap ((+) 1) hugeArrayTree |> arrayTreeExists
            , test "sequenceArrayThen doesn't overflow" <| \_ -> sequenceArrayThenMap ((+) 1) hugeArrayTree |> arrayTreeExists
            , test "traverseArray doesn't overflow" <| \_ -> traverseArrayMap ((+) 1) hugeArrayTree |> arrayTreeExists
            , test "traverseArrayThen doesn't overflow" <| \_ -> traverseArrayThenMap ((+) 1) hugeArrayTree |> arrayTreeExists
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
             , test "sequenceListThen" <| \_ -> Expect.equal expectedListTree (sequenceListThenMap ((+) 1) initListTree)
             , test "traverseList" <| \_ -> Expect.equal expectedListTree (traverseListMap ((+) 1) initListTree)
             , test "traverseListThen" <| \_ -> Expect.equal expectedListTree (traverseListThenMap ((+) 1) initListTree)
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
             , test "sequenceDictThen" <| \_ -> Expect.equal expectedDictTree (sequenceDictThenMap ((+) 1) initDictTree)
             , test "traverseDict" <| \_ -> Expect.equal expectedDictTree (traverseDictMap ((+) 1) initDictTree)
             , test "traverseDictThen" <| \_ -> Expect.equal expectedDictTree (traverseDictThenMap ((+) 1) initDictTree)
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
             , test "sequenceArrayThen" <| \_ -> Expect.equal expectedArrayTree (sequenceArrayThenMap ((+) 1) initArrayTree)
             , test "traverseArray" <| \_ -> Expect.equal expectedArrayTree (traverseArrayMap ((+) 1) initArrayTree)
             , test "traverseArrayThen" <| \_ -> Expect.equal expectedArrayTree (traverseArrayThenMap ((+) 1) initArrayTree)
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
             , test "sequenceMaybeThen" <| \_ -> Expect.equal expectedMaybeTree (sequenceMaybeThenMap ((+) 1) initMaybeTree)
             , test "traverseMaybe" <| \_ -> Expect.equal expectedMaybeTree (traverseMaybeMap ((+) 1) initMaybeTree)
             , test "traverseMaybeThen" <| \_ -> Expect.equal expectedMaybeTree (traverseMaybeThenMap ((+) 1) initMaybeTree)
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
             , test "sequenceResultThen" <| \_ -> Expect.equal expectedResultTree (sequenceResultThenMap ((+) 1) initResultTree)
             , test "traverseResult" <| \_ -> Expect.equal expectedResultTree (traverseResultMap ((+) 1) initResultTree)
             , test "traverseResultThen" <| \_ -> Expect.equal expectedResultTree (traverseResultThenMap ((+) 1) initResultTree)
             ]
            )
        ]


suite : Test
suite =
    describe "Recursion.Traverse"
        [ stackSafetyTests
        , correctnessTests
        ]
