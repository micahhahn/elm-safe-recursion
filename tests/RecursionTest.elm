module RecursionTest exposing (..)

import Expect
import Fuzz
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
                    (runRecursion (rec >> identity) list)
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


suite : Test
suite =
    describe "Recursion"
        [ safetyTests
        , functorLawTests
        , monadLawTests
        ]
