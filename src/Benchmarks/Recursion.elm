module Benchmarks.Recursion exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner as Runner exposing (BenchmarkProgram)
import Recursion exposing (..)


type Tree a
    = Leaf a
    | Node (Tree a) (Tree a)


type RoseTree a
    = RoseLeaf a
    | RoseNode (List (RoseTree a))


naiveMap : (a -> b) -> Tree a -> Tree b
naiveMap f tree =
    case tree of
        Leaf a ->
            Leaf (f a)

        Node l r ->
            Node (naiveMap f l) (naiveMap f r)


type TreeF a b
    = LeafF a
    | NodeF (Result (Tree a) (Tree b)) (Result (Tree a) (Tree b))


projectTree : Tree a -> TreeF a b
projectTree tree =
    case tree of
        Leaf a ->
            LeafF a

        Node l r ->
            NodeF (Err l) (Err r)


functorMap : (a -> b) -> Tree a -> Tree b
functorMap f tree =
    let
        go treeF stack =
            case treeF of
                LeafF a ->
                    let
                        x =
                            Leaf (f a)
                    in
                    case stack of
                        [] ->
                            x

                        item :: rest ->
                            case item of
                                NodeF (Err _) rr ->
                                    go (NodeF (Ok x) rr) rest

                                NodeF ll _ ->
                                    go (NodeF ll (Ok x)) rest

                                _ ->
                                    Debug.todo "Impossible"

                NodeF (Err todo) _ ->
                    go (projectTree todo) (treeF :: stack)

                NodeF _ (Err todo) ->
                    go (projectTree todo) (treeF :: stack)

                NodeF (Ok l) (Ok r) ->
                    let
                        x =
                            Node l r
                    in
                    case stack of
                        [] ->
                            x

                        item :: rest ->
                            case item of
                                NodeF (Err _) rr ->
                                    go (NodeF (Ok x) rr) rest

                                NodeF ll _ ->
                                    go (NodeF ll (Ok x)) rest

                                _ ->
                                    Debug.todo "Impossible"
    in
    go (projectTree tree) []


type Step a b
    = Done b
    | Todo a (b -> Step a b)


stepRecurse : (a -> Step a b) -> a -> b
stepRecurse project init =
    let
        go step stack =
            case step of
                Done b ->
                    case stack of
                        [] ->
                            b

                        next :: rest ->
                            go (next b) rest

                Todo a after ->
                    go (project a) (after :: stack)
    in
    go (project init) []


stepMap : (a -> b) -> Tree a -> Tree b
stepMap f =
    stepRecurse
        (\tree ->
            case tree of
                Leaf a ->
                    Done (Leaf (f a))

                Node l r ->
                    Todo l (\l_ -> Todo r (\r_ -> Done (Node l_ r_)))
        )


sequenceListStep : List a -> (List b -> Step a b) -> Step a b
sequenceListStep init finalize =
    let
        go todo done =
            case todo of
                [] ->
                    finalize done

                item :: items ->
                    Todo item (\b -> go items (b :: done))
    in
    go init []


foldListStep : (x -> ( Step a b, b -> z )) -> (z -> c -> c) -> c -> List x -> (c -> Step a b) -> Step a b
foldListStep project fold init items finalize =
    let
        go : List x -> c -> Step a b
        go todo accum =
            let
                unwrap embed rest step =
                    case step of
                        Done b ->
                            go rest (fold (embed b) accum)

                        Todo a f ->
                            Todo a (f >> unwrap embed rest)
            in
            case todo of
                [] ->
                    finalize accum

                item :: rest ->
                    let
                        ( step, embed ) =
                            project item
                    in
                    unwrap embed rest step
    in
    go items init


traverseListStep : List x -> (x -> ( Step a b, b -> z )) -> (List z -> Step a b) -> Step a b
traverseListStep init project finalize =
    let
        go : List x -> List z -> Step a b
        go todo done =
            let
                unwrap embed rest step =
                    case step of
                        Done b ->
                            go rest (embed b :: done)

                        Todo a f ->
                            Todo a (f >> unwrap embed rest)
            in
            case todo of
                [] ->
                    finalize done

                item :: items ->
                    let
                        ( step, embed ) =
                            project item
                    in
                    unwrap embed items step
    in
    go init []


type KeyRoseTree a
    = KeyRoseLeaf a
    | KeyRoseNode (List ( String, KeyRoseTree a ))


keyRoseTreeMap : (a -> b) -> KeyRoseTree a -> KeyRoseTree b
keyRoseTreeMap f =
    stepRecurse
        (\tree ->
            case tree of
                KeyRoseLeaf a ->
                    Done (KeyRoseLeaf (f a))

                KeyRoseNode items ->
                    -- traverseListStep items (\( key, t ) -> ( t, Tuple.pair key )) (KeyRoseNode >> Done)
                    Debug.todo ""
        )


roseTreeMap : (a -> b) -> RoseTree a -> RoseTree b
roseTreeMap f =
    stepRecurse
        (\tree ->
            case tree of
                RoseLeaf a ->
                    Done (RoseLeaf (f a))

                RoseNode items ->
                    sequenceListStep items (RoseNode >> Done)
        )


recMap : (a -> b) -> Tree a -> Tree b
recMap f =
    runRecursion
        (\tree ->
            case tree of
                Leaf a ->
                    base (Leaf (f a))

                Node l r ->
                    recurse l |> andThen (\l_ -> recurse r |> map (\r_ -> Node l_ r_))
        )


makeTree : Int -> Tree Int
makeTree =
    let
        go i depth =
            case depth of
                0 ->
                    Leaf i

                _ ->
                    let
                        l =
                            go i (depth - 1)

                        r =
                            go (i + (2 ^ (depth - 1))) (depth - 1)
                    in
                    Node l r
    in
    go 0


suite : Benchmark
suite =
    let
        tree =
            makeTree 8

        _ =
            Debug.log "tree" tree
    in
    describe "Map Implementations"
        [ benchmark "naiveMap" (\_ -> naiveMap ((+) 1) tree)
        , benchmark "recMap" (\_ -> recMap ((+) 1) tree)
        , benchmark "stepMap" (\_ -> stepMap ((+) 1) tree)
        , benchmark "functorMap" (\_ -> functorMap ((+) 1) tree)
        ]


main : BenchmarkProgram
main =
    Runner.program suite
