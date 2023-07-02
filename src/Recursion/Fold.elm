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
foldList : (t -> state -> state) -> state -> List r -> (state -> Rec r t) -> Rec r t
foldList fold state items after =
    case items of
        [] ->
            after state

        item :: rest ->
            recurse item (\t -> foldList fold (fold t state) rest after)


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
foldMapList : (x -> state -> (state -> Rec r t) -> Rec r t) -> state -> List x -> (state -> Rec r t) -> Rec r t
foldMapList foldMap state items after =
    case items of
        [] ->
            after state

        item :: rest ->
            foldMap item state (\s -> foldMapList foldMap s rest after)


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
foldDict : (comparable -> t -> state -> state) -> state -> Dict comparable r -> (state -> Rec r t) -> Rec r t
foldDict fold init dict after =
    let
        go todo accum =
            case todo of
                [] ->
                    after accum

                ( key, value ) :: rest ->
                    recurse value (\t -> go rest (fold key t accum))
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
foldMapDict : (comparable -> v -> state -> (state -> Rec r t) -> Rec r t) -> state -> Dict comparable v -> (state -> Rec r t) -> Rec r t
foldMapDict foldMap init dict after =
    let
        go todo state =
            case todo of
                [] ->
                    after state

                ( key, value ) :: rest ->
                    foldMap key value state (go rest)
    in
    go (Dict.toList dict) init


{-| Fold an `Array` whose items are recursive types.
-}
foldArray : (t -> state -> state) -> state -> Array r -> (state -> Rec r t) -> Rec r t
foldArray fold state items =
    foldList fold state (Array.toList items)


{-| Fold an `Array` whose items contain recursive types.
-}
foldMapArray : (a -> state -> (state -> Rec r t) -> Rec r t) -> state -> Array a -> (state -> Rec r t) -> Rec r t
foldMapArray foldMap accum items =
    foldMapList foldMap accum (Array.toList items)
