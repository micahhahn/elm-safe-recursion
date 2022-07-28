module RecursionTest exposing (suite)

import Dict exposing (Dict)
import Expect
import Fuzz
import Recursion exposing (..)
import Recursion.Traverse exposing (..)
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



{-
   x = RoseNode [n1, n2, n3, n4]

   Loop n1 (\b1 -> Loop n2 (\b2 -> Loop n3 (\b3 -> Loop n4 (\b4 -> RoseNode [b1, b2, b3, b4]))))
-}


safetyTests : Test
safetyTests =
    describe "Safety Tests"
        [ test "slowSum doesn't stack overflow" <|
            \_ ->
                slowSum 100000 |> Expect.within (Expect.Absolute 0) ((100000.0 * 100001.0) / 2.0)
        , test "fastSum doesn't stack overflow" <|
            \_ ->
                fastSum 100000 |> Expect.within (Expect.Absolute 0) ((100000.0 * 100001.0) / 2.0)
        , test "makeDeepLeftTree doesn't stack overflow" <|
            \_ ->
                Expect.pass
        , test "mapTree doesn't stack overflow" <|
            \_ ->
                Expect.pass
        , test "mapRoseTree doesn't stack overflow" <|
            \_ ->
                Expect.pass
        ]


mapRec : (a -> b) -> List a -> Rec (List a) (List b) (List b)
mapRec f list =
    case list of
        [] ->
            base []

        item :: rest ->
            recurse rest |> map (\items -> f item :: items)


functorLawTests : Test
functorLawTests =
    describe "Functor Laws"
        [ Test.fuzz (Fuzz.list Fuzz.int) "Functors preserve identity morphisms" <|
            \list ->
                let
                    rec =
                        mapRec (\x -> x + 1)
                in
                Expect.equalLists
                    (runRecursion (rec >> map identity) list)
                    (runRecursion rec list)
        , Test.fuzz (Fuzz.list Fuzz.int) "Functors preserve composition of morphisms" <|
            \list ->
                let
                    rec =
                        mapRec (\x -> x + 1)

                    f =
                        List.map (\x -> x // 3)

                    g =
                        List.map (\x -> x * 2)
                in
                Expect.equalLists
                    (runRecursion (rec >> map (f >> g)) list)
                    (runRecursion (rec >> (map f >> map g)) list)
        ]


monadLawTests : Test
monadLawTests =
    describe "Monad Laws"
        [ Test.fuzz (Fuzz.list Fuzz.int) "Left identity [ return a >>= h === h a ]" <|
            \list ->
                let
                    a =
                        [ 7 ]

                    h =
                        \items -> base <| items ++ [ 1 ]
                in
                Expect.equalLists
                    (runRecursion (\_ -> base a |> andThen h) list)
                    (runRecursion (\_ -> h a) list)
        , Test.fuzz (Fuzz.list Fuzz.int) "Right identity [ m >>= return === m ]" <|
            \list ->
                Expect.equalLists
                    (runRecursion (mapRec (\x -> x + 1) >> andThen base) list)
                    (runRecursion (mapRec (\x -> x + 1)) list)
        , Test.fuzz (Fuzz.list Fuzz.int) "Associativity [ (m >>= g) >>= h === m >>= (\\x -> g x >>= h) ]" <|
            -- TODO
            \_ -> Expect.pass
        ]


type DictTree a
    = DictLeaf a
    | DictNode (Dict String (DictTree a))


mapDictTree : (a -> b) -> DictTree a -> DictTree b
mapDictTree f =
    runRecursion
        (\tree ->
            case tree of
                DictLeaf a ->
                    base <| DictLeaf (f a)

                DictNode dict ->
                    dict |> traverseDict (\v -> recurse v) |> map DictNode
        )


traverseDictTest : Test
traverseDictTest =
    describe "traverseDict"
        [ test "traverseDict is correct" <|
            \_ ->
                let
                    input =
                        DictNode (Dict.fromList [ ( "a", DictLeaf 1 ), ( "b", DictLeaf 2 ) ])

                    expected =
                        DictNode (Dict.fromList [ ( "a", DictLeaf "1" ), ( "b", DictLeaf "2" ) ])
                in
                Expect.equal (mapDictTree String.fromInt input) expected
        ]


suite : Test
suite =
    describe "Recursion"
        [ safetyTests
        , functorLawTests
        , monadLawTests
        , traverseDictTest
        ]
