module Recursion.Fold exposing
    ( foldList, foldListThen
    , foldMapList, foldMapListThen
    , foldDict, foldDictThen
    , foldMapDict, foldMapDictThen
    , foldArray, foldArrayThen
    , foldMapArray, foldMapArrayThen
    )

{-| This module contains functions for folding common collections types that can contain recursive data structures.


## List

@docs foldList, foldListThen
@docs foldMapList, foldMapListThen


## Dict

@docs foldDict, foldDictThen
@docs foldMapDict, foldMapDictThen


## Array

@docs foldArray, foldArrayThen
@docs foldMapArray, foldMapArrayThen

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Recursion exposing (..)


{-| Fold a list of items which are recursive types.

    type RoseTree a
        = Leaf a
        | Node (List (RoseTree a))

    countRoseTreeLeaves : RoseTree a -> Int
    countRoseTreeLeaves =
        runRecursion <|
            \tree ->
                case tree of
                    Leaf _ ->
                        base 1

                    Node trees ->
                        foldList (+) 0 trees

-}
foldList : (t -> a -> a) -> a -> List r -> Rec r t a
foldList fold accum items =
    foldListThen fold accum items base


{-| Fold a list of items which are recurisve types and then perform a recursive action with the result.
-}
foldListThen : (t -> a -> a) -> a -> List r -> (a -> Rec r t b) -> Rec r t b
foldListThen fold accum items after =
    case items of
        [] ->
            after accum

        item :: rest ->
            recurseThen item (\t -> foldListThen fold (fold t accum) rest after)


{-| Fold a list of items which contain recursive types.

    type KeyedRoseTree a
        = Leaf a
        | Node (List ( String, KeyedRoseTree a ))

    countRoseTreeLeaves : KeyedRoseTree a -> Int
    countRoseTreeLeaves =
        runRecursion <|
            \tree ->
                case tree of
                    Leaf _ ->
                        base 1

                    Node trees ->
                        foldMapList
                            (\( _, rec ) count -> recurseMap rec ((+) count))
                            0
                            trees

-}
foldMapList : (x -> a -> Rec r t a) -> a -> List x -> Rec r t a
foldMapList foldMap accum items =
    foldMapListThen foldMap accum items base


{-| Fold a list of items which contain recursive types and then perform a recursive action with the result.
-}
foldMapListThen : (x -> a -> Rec r t a) -> a -> List x -> (a -> Rec r t b) -> Rec r t b
foldMapListThen foldMap accum items after =
    case items of
        [] ->
            after accum

        item :: rest ->
            foldMap item accum |> andThen (\a -> foldMapListThen foldMap a rest after)


{-| Fold a `Dict` whose values are recursive types.

    type HashTrie a
        = Leaf a
        | Node (Dict Char (HashTrie a))

    countHashTrie : HashTrie a -> Int
    countHashTrie =
        runRecursion <|
            \tree ->
                case tree of
                    Leaf _ ->
                        base 1

                    Node trees ->
                        foldDict (\_ x count -> x + count) 0 trees

-}
foldDict : (comparable -> t -> a -> a) -> a -> Dict comparable r -> Rec r t a
foldDict fold init dict =
    foldDictThen fold init dict base


{-| Fold a `Dict` whose values are recursive types and then perform a recursive action with the result.
-}
foldDictThen : (comparable -> t -> a -> a) -> a -> Dict comparable r -> (a -> Rec r t b) -> Rec r t b
foldDictThen fold init dict after =
    let
        go todo accum =
            case todo of
                [] ->
                    after accum

                ( key, value ) :: rest ->
                    recurseThen value (\t -> go rest (fold key t accum))
    in
    go (Dict.toList dict) init


{-| Fold a `Dict` whose values contain recursive types.

    type HashTrie a
        = Leaf a
        | Node (Dict Char ( Int, HashTrie a ))

    countHashTrie : HashTrie a -> Int
    countHashTrie =
        runRecursion <|
            \tree ->
                case tree of
                    Leaf _ ->
                        base 1

                    Node trees ->
                        foldMapDict (\_ ( _, v ) count -> recurseMap v (\x -> x + count)) 0 trees

-}
foldMapDict : (comparable -> v -> a -> Rec r t a) -> a -> Dict comparable v -> Rec r t a
foldMapDict foldMap init dict =
    let
        go todo accum =
            case todo of
                [] ->
                    base accum

                ( key, value ) :: rest ->
                    foldMap key value accum |> andThen (go rest)
    in
    go (Dict.toList dict) init


{-| Fold a `Dict` whose values contain recursive types and then perform a recursive action with the result.
-}
foldMapDictThen : (comparable -> v -> a -> Rec r t a) -> a -> Dict comparable v -> (a -> Rec r t b) -> Rec r t b
foldMapDictThen foldMap init dict after =
    let
        go todo accum =
            case todo of
                [] ->
                    after accum

                ( key, value ) :: rest ->
                    foldMap key value accum |> andThen (go rest)
    in
    go (Dict.toList dict) init


{-| Fold an `Array` whose items are recursive types.
-}
foldArray : (t -> a -> a) -> a -> Array r -> Rec r t a
foldArray fold accum items =
    foldList fold accum (Array.toList items)


{-| Fold an `Array` whose items are recursive types and then perform a recursive action with the result.
-}
foldArrayThen : (t -> a -> a) -> a -> Array r -> (a -> Rec r t b) -> Rec r t b
foldArrayThen fold accum items after =
    foldListThen fold accum (Array.toList items) after


{-| Fold an `Array` whose items contain recursive types.
-}
foldMapArray : (x -> a -> Rec r t a) -> a -> Array x -> Rec r t a
foldMapArray foldMap accum items =
    foldMapList foldMap accum (Array.toList items)


{-| Fold an `Array` whose items contain recursive types and then perform a recursive action with the result.
-}
foldMapArrayThen : (x -> a -> Rec r t a) -> a -> Array x -> (a -> Rec r t b) -> Rec r t b
foldMapArrayThen foldMap accum items after =
    foldMapListThen foldMap accum (Array.toList items) after
