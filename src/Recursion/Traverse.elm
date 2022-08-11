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
sequenceList : List a -> Rec a b (List b)
sequenceList items =
    foldList (::) [] (List.reverse items)


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
traverseList : (x -> Rec a b c) -> List x -> Rec a b (List c)
traverseList project items =
    foldMapList (\x cs -> project x |> map (\c -> c :: cs)) [] (List.reverse items)


{-| Traverse a `Dict` where the values are recursive types.
-}
sequenceDict : Dict comparable a -> Rec a b (Dict comparable b)
sequenceDict dict =
    foldDict (\k v cs -> ( k, v ) :: cs) [] dict
        |> map Dict.fromList


{-| Traverse a `Dict` where the values contain recursive types.
-}
traverseDict : (comparable -> v -> Rec a b c) -> Dict comparable v -> Rec a b (Dict comparable c)
traverseDict project dict =
    foldMapDict (\k v cs -> project k v |> map (\c -> ( k, c ) :: cs)) [] dict
        |> map Dict.fromList


{-| Traverse an `Array` where the values are recursive types.
-}
sequenceArray : Array a -> Rec a b (Array b)
sequenceArray items =
    sequenceList (Array.toList items)
        |> map Array.fromList


{-| Traverse an `Array` where the values contain recursive types.
-}
traverseArray : (x -> Rec a b c) -> Array x -> Rec a b (Array c)
traverseArray project items =
    traverseList project (Array.toList items)
        |> map Array.fromList


{-| Traverse a `Maybe` where the value might be a recursive type.
-}
sequenceMaybe : Maybe a -> Rec a b (Maybe b)
sequenceMaybe maybe =
    case maybe of
        Nothing ->
            base Nothing

        Just a ->
            recurse a |> map Just


{-| Traverse a `Maybe` where the value might contain a recursive type.
-}
traverseMaybe : (c -> Rec a b c) -> Maybe c -> Rec a b (Maybe c)
traverseMaybe project maybe =
    case maybe of
        Nothing ->
            base Nothing

        Just c ->
            project c |> map Just


{-| Traverse a `Result` where the success value might be a recursive type.
-}
sequenceResult : Result error a -> Rec a b (Result error b)
sequenceResult result =
    case result of
        Err err ->
            base (Err err)

        Ok a ->
            recurse a |> map Ok


{-| Traverse a `Result` where the success value might contain a recursive type.
-}
traverseResult : (value -> Rec a b c) -> Result error value -> Rec a b (Result error c)
traverseResult project result =
    case result of
        Err err ->
            base (Err err)

        Ok c ->
            project c |> map Ok
