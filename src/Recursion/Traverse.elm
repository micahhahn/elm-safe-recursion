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
        = Node a (List (RoseTree a))

    mapRoseTree : (a -> b) -> RoseTree a -> RoseTree b
    mapRoseTree f =
        runRecursion <|
            \(Node a nodes) ->
                sequenceList nodes
                    |> map (Node (f a))

-}
sequenceList : List r -> Rec r t (List t)
sequenceList items =
    foldList (::) [] (List.reverse items)


{-| Traverse a list where the elements contain recursive types.

    type KeyedRoseTree a
        = Node a (List ( String, KeyedRoseTree a ))

    mapKeyedRoseTree : (a -> b) -> KeyedRoseTree a -> KeyedRoseTree b
    mapKeyedRoseTree f =
        runRecursion <|
            \(Node a nodes) ->
                traverseList (\( s, tree ) -> recurseMap tree (Tuple.pair s)) nodes
                    |> map (Node (f a))

-}
traverseList : (x -> Rec r t a) -> List x -> Rec r t (List a)
traverseList project items =
    foldMapList (\x cs -> project x |> map (\c -> c :: cs)) [] (List.reverse items)


{-| Traverse a `Dict` where the values are recursive types.

    type HashTrie a
        = Leaf a
        | Node (Dict Char (HashTrie a))

    mapHashTrie : (a -> b) -> HashTrie a -> HashTrie b
    mapHashTrie f =
        runRecursion <|
            \tree ->
                case tree of
                    Leaf a ->
                        base (Leaf (f a))

                    Node dict ->
                        sequenceDict dict
                            |> map Node

-}
sequenceDict : Dict comparable r -> Rec r t (Dict comparable t)
sequenceDict dict =
    foldDict (\k v cs -> ( k, v ) :: cs) [] dict
        |> map Dict.fromList


{-| Traverse a `Dict` where the values contain recursive types.
-}
traverseDict : (comparable -> v -> Rec r t a) -> Dict comparable v -> Rec r t (Dict comparable a)
traverseDict project dict =
    foldMapDict (\k v cs -> project k v |> map (\c -> ( k, c ) :: cs)) [] dict
        |> map Dict.fromList


{-| Traverse an `Array` where the values are recursive types.
-}
sequenceArray : Array r -> Rec r t (Array t)
sequenceArray items =
    sequenceList (Array.toList items)
        |> map Array.fromList


{-| Traverse an `Array` where the values contain recursive types.
-}
traverseArray : (x -> Rec r t a) -> Array x -> Rec r t (Array a)
traverseArray project items =
    traverseList project (Array.toList items)
        |> map Array.fromList


{-| Traverse a `Maybe` where the value might be a recursive type.

    type NonEmpty a
        = NonEmpty a (Maybe (NonEmpty a))

    mapNonEmpty : (a -> b) -> NonEmpty a -> NonEmpty b
    mapNonEmpty f =
        runRecursion <|
            \(NonEmpty v maybe) ->
                sequenceMaybe maybe
                    |> map (NonEmpty (f v))

-}
sequenceMaybe : Maybe r -> Rec r t (Maybe t)
sequenceMaybe maybe =
    case maybe of
        Nothing ->
            base Nothing

        Just a ->
            recurseThen a (Just >> base)


{-| Traverse a `Maybe` where the value might contain a recursive type.

    type SeparatedList sep val
        = SeparatedList val (Maybe ( sep, SeparatedList sep val ))

    mapSeparatedList : (a -> b) -> SeparatedList sep a -> SeparatedList sep b
    mapSeparatedList f =
        runRecursion <|
            \(SeparatedList a maybeOthers) ->
                maybeOthers
                    |> traverseMaybe (\( sep, sepList ) -> recurseMap sepList (Tuple.pair sep))
                    |> map (SeparatedList (f a))

-}
traverseMaybe : (x -> Rec r t a) -> Maybe x -> Rec r t (Maybe a)
traverseMaybe project maybe =
    case maybe of
        Nothing ->
            base Nothing

        Just c ->
            project c |> map Just


{-| Traverse a `Result` where the success value might be a recursive type.
-}
sequenceResult : Result e r -> Rec r t (Result e t)
sequenceResult result =
    case result of
        Err err ->
            base (Err err)

        Ok a ->
            recurseThen a (Ok >> base)


{-| Traverse a `Result` where the success value might contain a recursive type.
-}
traverseResult : (v -> Rec r t a) -> Result e v -> Rec r t (Result e a)
traverseResult project result =
    case result of
        Err err ->
            base (Err err)

        Ok c ->
            project c |> map Ok
