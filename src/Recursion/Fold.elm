module Recursion.Fold exposing
    ( foldList, foldMapList
    , foldDict, foldMapDict
    , foldArray, foldMapArray
    )

{-| This module contains functions for folding common collections types that can contain recursive data structures.

Prefer to use the functions that accept a continuation when possible (`fold____Then`) as they will be more efficient than folding and then mapping after.


# List

@docs foldList, foldMapList


# Dict

@docs foldDict, foldMapDict


# Array

@docs foldArray, foldMapArray

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
    case items of
        [] ->
            base accum

        item :: rest ->
            recurse item |> andThen (\t -> foldList fold (fold t accum) rest)


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
    case items of
        [] ->
            base accum

        item :: rest ->
            foldMap item accum |> andThen (\a -> foldMapList foldMap a rest)


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
    let
        go todo accum =
            case todo of
                [] ->
                    base accum

                ( key, value ) :: rest ->
                    recurse value |> andThen (\t -> go rest (fold key t accum))
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


{-| Fold an `Array` whose items are recursive types.
-}
foldArray : (t -> a -> a) -> a -> Array r -> Rec r t a
foldArray fold accum items =
    foldList fold accum (Array.toList items)


{-| Fold an `Array` whose items contain recursive types.
-}
foldMapArray : (x -> a -> Rec r t a) -> a -> Array x -> Rec r t a
foldMapArray foldMap accum items =
    foldMapList foldMap accum (Array.toList items)
