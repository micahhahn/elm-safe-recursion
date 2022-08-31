module Recursion.Traverse exposing
    ( sequenceList, sequenceListThen
    , traverseList, traverseListThen
    , sequenceDict, sequenceDictThen
    , traverseDict, traverseDictThen
    , sequenceArray, sequenceArrayThen
    , traverseArray, traverseArrayThen
    , sequenceMaybe, sequenceMaybeThen
    , traverseMaybe, traverseMaybeThen
    , sequenceResult, sequenceResultThen
    , traverseResult, traverseResultThen
    )

{-| This module provides traversals for common data structures that contain recursive types.


### What is a traversal?

A traversal is a transformation over a structure that **preserves the shape** of the structure.
In this module, the traversal functions allow us to convert from a structure containing recursive types into a recursive type containing the structure.

If you are trying to write a map function over a recursive data structure, a traversal is likely what you want.

## List

@docs sequenceList, sequenceListThen
@docs traverseList, traverseListThen


## Dict

@docs sequenceDict, sequenceDictThen
@docs traverseDict, traverseDictThen


## Array

@docs sequenceArray, sequenceArrayThen
@docs traverseArray, traverseArrayThen


## Maybe

@docs sequenceMaybe, sequenceMaybeThen
@docs traverseMaybe, traverseMaybeThen


## Result

@docs sequenceResult, sequenceResultThen
@docs traverseResult, traverseResultThen

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
    sequenceListThen items base

{-| Traverse a list where the elements are recursive types and then perform a recursive action on the result.
-}
sequenceListThen : List r -> (List t -> Rec r t a) -> Rec r t a
sequenceListThen items after =
    foldListThen (::) [] (List.reverse items) after


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
    traverseListThen project items base

{-| Traverse a list where the elements contain recursive types and then perform a recursive action on the result.
-}
traverseListThen : (x -> Rec r t a) -> List x -> (List a -> Rec r t b) -> Rec r t b
traverseListThen project items after =
    let
        go accum todo =
            case todo of
                [] ->
                    after <| List.reverse accum

                item :: rest ->
                    project item |> andThen (\a -> go (a :: accum) rest)
    in
    go [] items


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
    sequenceDictThen dict base

{-| Traverse a `Dict` where the values are recursive types and then perform a recursive action on the result.
-}
sequenceDictThen : Dict comparable r -> (Dict comparable t -> Rec r t a) -> Rec r t a
sequenceDictThen dict after =
    let
        go accum todo =
            case todo of
                [] ->
                    after (Dict.fromList accum)

                ( key, value ) :: rest ->
                    recurseThen value (\t -> go (( key, t ) :: accum) rest)
    in
    go [] (Dict.toList dict)


{-| Traverse a `Dict` where the values contain recursive types.
-}
traverseDict : (comparable -> v -> Rec r t a) -> Dict comparable v -> Rec r t (Dict comparable a)
traverseDict project dict =
    traverseDictThen project dict base

{-| Traverse a `Dict` where the values contain recursive types and then perform a recursive action the result.
-}
traverseDictThen : (comparable -> v -> Rec r t a) -> Dict comparable v -> (Dict comparable a -> Rec r t b) -> Rec r t b
traverseDictThen project dict after =
    let
        go accum todo =
            case todo of
                [] ->
                    after (Dict.fromList accum)

                ( key, value ) :: rest ->
                    project key value |> andThen (\a -> go (( key, a ) :: accum) rest)
    in
    go [] (Dict.toList dict)


{-| Traverse an `Array` where the values are recursive types.
-}
sequenceArray : Array r -> Rec r t (Array t)
sequenceArray items =
    sequenceArrayThen items base

{-| Traverse an `Array` where the values are recursive types and then perform a recursive action on the result.
-}
sequenceArrayThen : Array r -> (Array t -> Rec r t a) -> Rec r t a
sequenceArrayThen items after =
    sequenceListThen (Array.toList items) (Array.fromList >> after)


{-| Traverse an `Array` where the values contain recursive types.
-}
traverseArray : (x -> Rec r t a) -> Array x -> Rec r t (Array a)
traverseArray project items =
    traverseArrayThen project items base

{-| Traverse an `Array` where the values contain recursive types and then perform a recursive action on the result.
-}
traverseArrayThen : (x -> Rec r t a) -> Array x -> (Array a -> Rec r t b) -> Rec r t b
traverseArrayThen project items after =
    traverseListThen project (Array.toList items) (Array.fromList >> after)


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
    sequenceMaybeThen maybe base

{-| Traverse a `Maybe` where the value might be a recursive type and then perform a recursive action on the result.
-}
sequenceMaybeThen : Maybe r -> (Maybe t -> Rec r t a) -> Rec r t a
sequenceMaybeThen maybe after = 
    case maybe of
        Nothing -> 
            after Nothing

        Just r -> 
            recurseThen r (Just >> after)

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
    traverseMaybeThen project maybe base

{-| Traverse a `Maybe` where the value might contain a recursive type and then perform a recursive action on the result.
-}
traverseMaybeThen : (x -> Rec r t a) -> Maybe x -> (Maybe a -> Rec r t b) -> Rec r t b
traverseMaybeThen project maybe after = 
    case maybe of
        Nothing -> after Nothing
        Just x -> 
            project x |> andThen (Just >> after)

{-| Traverse a `Result` where the success value might be a recursive type.
-}
sequenceResult : Result e r -> Rec r t (Result e t)
sequenceResult result =
    sequenceResultThen result base

{-| Traverse a `Result` where the success value might be a recursive type and then perform an action on the recursive result.
-}
sequenceResultThen : Result e r -> (Result e t -> Rec r t a) -> Rec r t a
sequenceResultThen result after = 
    case result of
        Err err ->
            after (Err err)

        Ok a ->
            recurseThen a (Ok >> after)


{-| Traverse a `Result` where the success value might contain a recursive type.
-}
traverseResult : (v -> Rec r t a) -> Result e v -> Rec r t (Result e a)
traverseResult project result =
    case result of
        Err err ->
            base (Err err)

        Ok c ->
            project c |> map Ok

{-| Traverse a `Result` where the success value might contain a recursive type and then perform an action on the recursive result.
-}
traverseResultThen : (v -> Rec r t a) -> Result e v -> (Result e a -> Rec r t b) -> Rec r t b
traverseResultThen project result after =
    case result of
        Err err ->
            after (Err err)

        Ok c ->
            project c |> andThen (Ok >> after)