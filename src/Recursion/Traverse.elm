module Recursion.Traverse exposing
    ( sequenceList, traverseList
    , sequenceDict, traverseDict
    , sequenceArray, traverseArray
    , sequenceMaybe, traverseMaybe
    , sequenceResult, traverseResult
    )

{-| This module provides traversals for common data structures that contain recursive types.


### What is a traversal?

A traversal is a transformation over a structure that **preserves the shape** of the structure.
In this module, the traversal functions allow us to convert from a structure containing recursive types into a recursive type containing the structure.

If you are trying to write a map function over a recursive data structure, a traversal is likely what you want.


## List

@docs sequenceList, traverseList


## Dict

@docs sequenceDict, traverseDict


## Array

@docs sequenceArray, traverseArray


## Maybe

@docs sequenceMaybe, traverseMaybe


## Result

@docs sequenceResult, traverseResult

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Recursion exposing (..)
import Recursion.Fold exposing (..)


{-| Traverse a list where the elements are recursive types.

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
                        sequenceList nodes (Node >> base)

-}
sequenceList : List a -> (List b -> Step a b) -> Step a b
sequenceList items after =
    foldList (::) [] items (List.reverse >> after)


{-| Traverse a list where the elements contain recursive types.

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
                        traverseList
                            (\( key, node ) -> ( recurse node, Tuple.pair key ))
                            nodes
                            (Node >> base)

-}
traverseList : (c -> ( Step a b, b -> d )) -> List c -> (List d -> Step a b) -> Step a b
traverseList project items after =
    foldMapList (project >> Tuple.mapSecond ((<<) (::))) [] items (List.reverse >> after)


{-| Traverse a `Dict` where the values are recursive types.
-}
sequenceDict : Dict comparable a -> (Dict comparable b -> Step a b) -> Step a b
sequenceDict dict after =
    foldMapList (\( k, v ) -> ( recurse v, Tuple.pair k >> (::) )) [] (Dict.toList dict) (Dict.fromList >> after)


{-| Traverse a `Dict` where the values contain recursive types.
-}
traverseDict : (comparable -> c -> ( Step a b, b -> d )) -> Dict comparable c -> (Dict comparable d -> Step a b) -> Step a b
traverseDict project dict after =
    traverseList (\( k, v ) -> project k v |> Tuple.mapSecond ((<<) (Tuple.pair k))) (Dict.toList dict) (Dict.fromList >> after)


{-| Traverse an `Array` where the values are recursive types.
-}
sequenceArray : Array a -> (Array b -> Step a b) -> Step a b
sequenceArray items after =
    sequenceList (Array.toList items) (Array.fromList >> after)


{-| Traverse an `Array` where the values contain recursive types.
-}
traverseArray : (c -> ( Step a b, b -> d )) -> Array c -> (Array d -> Step a b) -> Step a b
traverseArray project items after =
    traverseList project (Array.toList items) (Array.fromList >> after)


{-| Traverse a `Maybe` where the value might be a recursive type.
-}
sequenceMaybe : Maybe a -> (Maybe b -> Step a b) -> Step a b
sequenceMaybe maybe after =
    case maybe of
        Nothing ->
            after Nothing

        Just a ->
            recurse a |> andThen (Just >> after)


{-| Traverse a `Maybe` where the value might contain a recursive type.
-}
traverseMaybe : (c -> ( Step a b, b -> d )) -> Maybe c -> (Maybe d -> Step a b) -> Step a b
traverseMaybe project maybe after =
    case maybe of
        Nothing ->
            after Nothing

        Just c ->
            let
                ( step, map ) =
                    project c
            in
            step |> andThen (map >> Just >> after)


{-| Traverse a `Result` where the success value might be a recursive type.
-}
sequenceResult : Result error a -> (Result error b -> Step a b) -> Step a b
sequenceResult result after =
    case result of
        Err err ->
            after (Err err)

        Ok a ->
            recurse a |> andThen (Ok >> after)


{-| Traverse a `Result` where the success value might contain a recursive type.
-}
traverseResult : (c -> ( Step a b, b -> d )) -> Result error c -> (Result error d -> Step a b) -> Step a b
traverseResult project result after =
    case result of
        Err err ->
            after (Err err)

        Ok c ->
            let
                ( step, map ) =
                    project c
            in
            step |> andThen (map >> Ok >> after)
