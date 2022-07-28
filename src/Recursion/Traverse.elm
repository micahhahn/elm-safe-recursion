module Recursion.Traverse exposing
    ( traverseList, sequenceList
    , traverseDict, sequenceDict
    , traverseMaybe, sequenceMaybe
    , traverseResult, sequenceResult
    , traverseArray, sequenceArray
    )

{-| This module provides traversals for common data structures with the `Rec` type.


### What is a traversal?

A traversal is a transformation over a structure that **preserves the shape** of the structure.
In this module, the traversal functions allow us to convert from a structure containing recursive types into a recursive type containing the structure.

If you are trying to write a map function over a recursive data structure, a traversal is likely what you want.


## List

@docs traverseList, sequenceList


## Dict

@docs traverseDict, sequenceDict


## Maybe

@docs traverseMaybe, sequenceMaybe


## Result

@docs traverseResult, sequenceResult


## Array

@docs traverseArray, sequenceArray

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Recursion exposing (..)
import Recursion.Fold exposing (foldrList)


{-| Traverse a list of recursive data types.

    type KeyedRoseTree a
        = Leaf a
        | Node (List ( String, KeyedRoseTree a ))

    mapKeyedRoseTree : (a -> b) -> KeyedRoseTree a -> KeyedRoseTree b
    mapKeyedRoseTree f =
        runRecursion <|
            \tree ->
                case tree of
                    Leaf a ->
                        base <| Leaf (f a)

                    Node nodes ->
                        nodes
                            |> traverseList
                                (\( key, node ) ->
                                    recurse node |> map (Tuple.pair key)
                                )
                            |> map Node

-}
traverseList : (x -> Rec a b c) -> List x -> Rec a b (List c)
traverseList project list =
    foldrList project (::) [] list


{-| A specialization of `traverseList` when each element of the list is directly recursive.

`sequenceList = traverseList recurse`

    type RoseTree a
        = Leaf a
        | Node (List (RoseTree a))

    mapRoseTree : (a -> b) -> RoseTree a -> RoseTree b
    mapRoseTree f =
        runRecursion <|
            \tree ->
                case tree of
                    Leaf a ->
                        base <| Leaf (f a)

                    Node nodes ->
                        nodes
                            |> sequenceList
                            |> map Node

-}
sequenceList : List a -> Rec a b (List b)
sequenceList =
    traverseList recurse


{-| Traverse over a `Dict` where the values contain an recursive type.
-}
traverseDict : (v -> Rec a b c) -> Dict comparable v -> Rec a b (Dict comparable c)
traverseDict project dict =
    Dict.toList dict
        |> traverseList
            (\( k, v ) ->
                project v
                    |> map (Tuple.pair k)
            )
        |> map Dict.fromList


{-| Sequence a `Dict` where the values directly are recursive types.

`sequenceDict = traverseDict recurse`

-}
sequenceDict : Dict comparable v -> Rec v b (Dict comparable b)
sequenceDict =
    traverseDict recurse


{-| Traverse a maybe containing a recursive data type.

    type SeparatedList sep val
        = SeparatedList val (Maybe ( sep, SeparatedList sep val ))

    mapSeparatedList : (a -> b) -> SeparatedList sep a -> SeparatedList sep b
    mapSeparatedList f =
        runRecursion <|
            \(SeparatedList a maybeOthers) ->
                maybeOthers
                    |> traverseMaybe (\( sep, sepList ) -> recurse sepList |> map (Tuple.pair sep))
                    |> map (SeparatedList (f a))

-}
traverseMaybe : (x -> Rec a b c) -> Maybe x -> Rec a b (Maybe c)
traverseMaybe f maybe =
    case maybe of
        Nothing ->
            base Nothing

        Just x ->
            f x |> map Just


{-| A specialization of `traverseMaybe` for when the `Maybe` type is directly recursive.

`sequenceMaybe = traverseMaybe recurse`

    type NonEmpty a
        = NonEmpty a (Maybe (NonEmpty a))

    mapNonEmpty : (a -> b) -> NonEmpty a -> NonEmpty b
    mapNonEmpty f =
        runRecursion <|
            \(NonEmpty a maybe) ->
                maybe
                    |> sequenceMaybe
                    |> map (NonEmpty (f a))

-}
sequenceMaybe : Maybe a -> Rec a b (Maybe b)
sequenceMaybe =
    traverseMaybe recurse


{-| Traverse over a `Result` where the success value can contain a recursive type.
-}
traverseResult : (value -> Rec a b c) -> Result error value -> Rec a b (Result error c)
traverseResult f result =
    case result of
        Err e ->
            base (Err e)

        Ok v ->
            f v |> map Ok


{-| Sequence a `Result` where the success value directly is a recursive type.

`sequenceResult = traverseResult recurse`

-}
sequenceResult : Result error value -> Rec value b (Result error b)
sequenceResult =
    traverseResult recurse


{-| Traverse an array of recursive data types.

    type KeyedRoseTree a
        = Leaf a
        | Node (Array ( String, KeyedRoseTree a ))

    mapKeyedRoseTree : (a -> b) -> KeyedRoseTree a -> KeyedRoseTree b
    mapKeyedRoseTree f =
        runRecursion <|
            \tree ->
                case tree of
                    Leaf a ->
                        base <| Leaf (f a)

                    Node nodes ->
                        nodes
                            |> traverseArray
                                (\( key, node ) ->
                                    recurse node |> map (Tuple.pair key)
                                )
                            |> map Node

-}
traverseArray : (x -> Rec a b c) -> Array x -> Rec a b (Array c)
traverseArray f =
    Array.toList >> traverseList f >> map Array.fromList


{-| A specialization of `traverseArray` when each element of the list is directly recursive.

`sequenceArray = traverseArray recurse`

    type RoseTree a
        = Leaf a
        | Node (Array (RoseTree a))

    mapRoseTree : (a -> b) -> RoseTree a -> RoseTree b
    mapRoseTree f =
        runRecursion <|
            \tree ->
                case tree of
                    Leaf a ->
                        base <| Leaf (f a)

                    Node nodes ->
                        nodes
                            |> sequenceArray
                            |> map Node

-}
sequenceArray : Array a -> Rec a b (Array b)
sequenceArray =
    traverseArray recurse
