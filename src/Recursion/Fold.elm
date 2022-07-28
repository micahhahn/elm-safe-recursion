module Recursion.Fold exposing
    ( foldlList, foldrList
    , foldlDict, foldrDict
    , foldlArray, foldrArray
    )

{-| This module contains functions for folding common collections types that can contain recursive data structures.


## List

@docs foldlList, foldrList


## Dict

@docs foldlDict, foldrDict


## Array

@docs foldlArray, foldrArray

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Recursion exposing (..)


{-| Reduces a list that contains recursive types from left to right.

    type RoseTree a
        = Leaf a
        | Node (List (RoseTree a))

    countRoseTreeLeaves : RoseTree a -> Int
    countRoseTreeLeaves =
        runRecursion
            (\tree ->
                case tree of
                    Leaf _ ->
                        base 1

                    Node trees ->
                        foldlList recurse (+) 0 trees
            )

-}
foldlList : (x -> Rec a b c) -> (c -> d -> d) -> d -> List x -> Rec a b d
foldlList project fold init list =
    let
        go todos done =
            case todos of
                item :: rest ->
                    project item |> andThen (\newItem -> go rest (fold newItem done))

                _ ->
                    base done
    in
    go list init


{-| Fold a list that contains recursive types from right to left.

    type RoseTree a
        = Leaf a
        | Node (List (RoseTree a))

    countRoseTreeLeaves : RoseTree a -> Int
    countRoseTreeLeaves =
        runRecursion
            (\tree ->
                case tree of
                    Leaf _ ->
                        base 1

                    Node trees ->
                        foldrList recurse (+) 0 trees
            )

Favor `foldlList` if possible as it will be more efficient.

-}
foldrList : (x -> Rec a b c) -> (c -> d -> d) -> d -> List x -> Rec a b d
foldrList project fold init list =
    foldlList project fold init (List.reverse list)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


{-| Fold a dictionary that contains recursive types in its value type in ascending order of keys.
-}
foldlDict : (comparable -> v -> Rec a b c) -> (c -> d -> d) -> d -> Dict comparable v -> Rec a b d
foldlDict project fold init =
    Dict.toList >> foldlList (uncurry project) fold init


{-| Fold a dictionary that contains recursive types in its value type in descending order of keys.
-}
foldrDict : (comparable -> v -> Rec a b c) -> (c -> d -> d) -> d -> Dict comparable v -> Rec a b d
foldrDict project fold init =
    Dict.toList >> foldrList (uncurry project) fold init


{-| Fold an array that contains recursive types from left to right.
-}
foldlArray : (x -> Rec a b c) -> (c -> d -> d) -> d -> Array x -> Rec a b d
foldlArray project fold init =
    Array.toList >> foldlList project fold init


{-| Fold an array that contains recursive types from right to left.
-}
foldrArray : (x -> Rec a b c) -> (c -> d -> d) -> d -> Array x -> Rec a b d
foldrArray project fold init =
    Array.toList >> foldrList project fold init
