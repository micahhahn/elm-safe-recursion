module Recursion.Traverse exposing
    ( sequenceList, traverseList
    , sequenceDict, traverseDict
    , sequenceArray, traverseArray
    , sequenceMaybe, traverseMaybe
    , sequenceResult, traverseResult
    )

{-| This module provides traversals for common data structures that contain recursive types.

Prefer to use the functions that accept a continuation when possible (`sequence____Then` or `traverse____Then`) as they will be more efficient than folding and then mapping after.


### What is a traversal?

A traversal is a transformation over a structure that **preserves the shape** of the structure.
In this module, the traversal functions allow us to convert from a structure containing recursive types into a recursive type containing the structure.

If you are trying to write a map function over a recursive data structure, a traversal is likely what you want.


# List

@docs sequenceList, traverseList


# Dict

@docs sequenceDict, traverseDict


# Array

@docs sequenceArray, traverseArray


# Maybe

@docs sequenceMaybe, traverseMaybe


# Result

@docs sequenceResult, traverseResult

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Recursion exposing (..)
import Recursion.Fold exposing (..)


sequenceList : List r -> (List t -> Rec r t) -> Rec r t
sequenceList =
    traverseList recurse


{-| Traverse a list where the elements contain recursive types and then perform a recursive action on the result.
-}
traverseList : (x -> (a -> Rec r t) -> Rec r t) -> List x -> (List a -> Rec r t) -> Rec r t
traverseList project items after =
    let
        go accum todo =
            case todo of
                [] ->
                    after <| List.reverse accum

                item :: rest ->
                    project item (\a -> go (a :: accum) rest)
    in
    go [] items


sequenceDict : Dict comparable r -> (Dict comparable t -> Rec r t) -> Rec r t
sequenceDict =
    traverseDict (always recurse)


traverseDict : (comparable -> value -> (a -> Rec r t) -> Rec r t) -> Dict comparable value -> (Dict comparable a -> Rec r t) -> Rec r t
traverseDict project dict after =
    let
        go accum todo =
            case todo of
                [] ->
                    after (Dict.fromList accum)

                ( key, value ) :: rest ->
                    project key value (\a -> go (( key, a ) :: accum) rest)
    in
    go [] (Dict.toList dict)


sequenceArray : Array r -> (Array t -> Rec r t) -> Rec r t
sequenceArray =
    traverseArray recurse


traverseArray : (x -> (a -> Rec r t) -> Rec r t) -> Array x -> (Array a -> Rec r t) -> Rec r t
traverseArray project array after =
    traverseList project (Array.toList array) (Array.fromList >> after)


sequenceMaybe : Maybe r -> (Maybe t -> Rec r t) -> Rec r t
sequenceMaybe =
    traverseMaybe recurse


traverseMaybe : (x -> (a -> Rec r t) -> Rec r t) -> Maybe x -> (Maybe a -> Rec r t) -> Rec r t
traverseMaybe project maybe after =
    case maybe of
        Nothing ->
            after Nothing

        Just x ->
            project x (Just >> after)


sequenceResult : Result err r -> (Result err t -> Rec r t) -> Rec r t
sequenceResult =
    traverseResult recurse


traverseResult : (ok -> (a -> Rec r t) -> Rec r t) -> Result err ok -> (Result err a -> Rec r t) -> Rec r t
traverseResult project result after =
    case result of
        Err err ->
            after (Err err)

        Ok ok ->
            project ok (Ok >> after)
