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


{-| Fold a list of recursive types.
-}
foldList : (b -> c -> c) -> c -> List a -> (c -> Step a b) -> Step a b
foldList fold init items after =
    let
        go todo accum =
            case todo of
                [] ->
                    after accum

                item :: rest ->
                    recurse item |> andThen (\b -> go rest (fold b accum))
    in
    go items init


{-| Fold a list that contains recursive types with a selector.
-}
foldMapList : (c -> ( Step a b, b -> d -> d )) -> d -> List c -> (d -> Step a b) -> Step a b
foldMapList project init items after =
    let
        go : List c -> d -> Step a b
        go todo accum =
            case todo of
                [] ->
                    after accum

                item :: rest ->
                    let
                        ( step, fold ) =
                            project item
                    in
                    step
                        |> andThen (\b -> go rest (fold b accum))
    in
    go items init


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


{-| Fold a Dict with recursive value types.
-}
foldDict : (comparable -> b -> c -> c) -> c -> Dict comparable a -> (c -> Step a b) -> Step a b
foldDict fold init dict after =
    foldMapList (\( k, a ) -> ( recurse a, fold k )) init (Dict.toList dict) after


{-| Fold a Dict with recursive value types with a selector.
-}
foldMapDict : (comparable -> c -> ( Step a b, b -> d -> d )) -> d -> Dict comparable c -> (d -> Step a b) -> Step a b
foldMapDict fold init dict after =
    foldMapList (uncurry fold) init (Dict.toList dict) after


{-| Fold an array of recursive types.
-}
foldArray : (b -> c -> c) -> c -> Array a -> (c -> Step a b) -> Step a b
foldArray fold init items after =
    foldList fold init (Array.toList items) after


{-| Fold an array that contains recursive types with a selector.
-}
foldMapArray : (c -> ( Step a b, b -> d -> d )) -> d -> Array c -> (d -> Step a b) -> Step a b
foldMapArray fold init items after =
    foldMapList fold init (Array.toList items) after


{-| Fold a set of recursive types.
-}
foldSet : (b -> c -> c) -> c -> Set a -> (c -> Step a b) -> Step a b
foldSet fold init items after =
    foldList fold init (Set.toList items) after


{-| Fold a set that contains recursive types with a selector.
-}
foldMapSet : (c -> ( Step a b, b -> d -> d )) -> d -> Set c -> (d -> Step a b) -> Step a b
foldMapSet fold init items after =
    foldMapList fold init (Set.toList items) after
