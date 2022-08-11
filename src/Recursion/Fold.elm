module Recursion.Fold exposing
    ( foldList, foldMapList
    , foldDict, foldMapDict
    , foldArray, foldMapArray
    , foldSet, foldMapSet
    )

{-| This module contains functions for folding common collections types that can contain recursive data structures.


## List

@docs foldList, foldMapList


## Dict

@docs foldDict, foldMapDict


## Array

@docs foldArray, foldMapArray


## Set

@docs foldSet, foldMapSet

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Recursion exposing (..)
import Set exposing (Set)


{-| Fold a list of items which are recursive types.
-}
foldList : (b -> c -> c) -> c -> List a -> Rec a b c
foldList fold accum items =
    case items of
        [] ->
            base accum

        item :: rest ->
            recurse item |> andThen (\b -> foldList fold (fold b accum) rest)


{-| Fold a list of items which contain recursive types.
-}
foldMapList : (x -> c -> Rec a b c) -> c -> List x -> Rec a b c
foldMapList foldMap accum items =
    case items of
        [] ->
            base accum

        item :: rest ->
            foldMap item accum |> andThen (\c -> foldMapList foldMap c rest)


{-| Fold a `Dict` whose values are recursive types.
-}
foldDict : (comparable -> b -> c -> c) -> c -> Dict comparable a -> Rec a b c
foldDict fold init dict =
    let
        go todo accum =
            case todo of
                [] ->
                    base accum

                ( key, value ) :: rest ->
                    recurse value |> andThen (\b -> go rest (fold key b accum))
    in
    go (Dict.toList dict) init


{-| Fold a `Dict` whose values contain recursive types.
-}
foldMapDict : (comparable -> v -> c -> Rec a b c) -> c -> Dict comparable v -> Rec a b c
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
foldArray : (b -> c -> c) -> c -> Array a -> Rec a b c
foldArray fold accum items =
    foldList fold accum (Array.toList items)


{-| Fold an `Array` whose items contain recursive types.
-}
foldMapArray : (x -> c -> Rec a b c) -> c -> Array x -> Rec a b c
foldMapArray foldMap accum items =
    foldMapList foldMap accum (Array.toList items)


{-| Fold an `Set` whose items are recursive types.
-}
foldSet : (b -> c -> c) -> c -> Set a -> Rec a b c
foldSet fold accum items =
    foldList fold accum (Set.toList items)


{-| Fold an `Set` whose items contain recursive types.
-}
foldMapSet : (x -> c -> Rec a b c) -> c -> Set x -> Rec a b c
foldMapSet foldMap accum items =
    foldMapList foldMap accum (Set.toList items)
