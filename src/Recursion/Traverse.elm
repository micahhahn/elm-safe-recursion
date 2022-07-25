module Recursion.Traverse exposing
    ( traverseList, sequenceList
    , traverseDict, sequenceDict
    , traverseMaybe, sequenceMaybe
    , traverseResult, sequenceResult
    , traverseArray, sequenceArray
    )

{-|


## List

@docs traverseList, sequenceList


## Dict

@docs traverseDict, sequenceDict


## Maybe

@docs traverseMaybe, sequenceMaybe


## Result

@docs traverseResult, sequenceResult


## Array

@docs traverseArray, sequenceArray

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Recursion exposing (..)
import Recursion.Fold exposing (foldrList)


{-| -}
traverseList : (x -> Rec a b c) -> List x -> Rec a b (List c)
traverseList project list =
    foldrList project (::) [] list


{-| -}
sequenceList : List a -> Rec a b (List b)
sequenceList =
    traverseList recurse


{-| -}
traverseDict : (v -> Rec a b c) -> Dict comparable v -> Rec a b (Dict comparable c)
traverseDict project dict =
    Dict.toList dict
        |> traverseList
            (\( k, v ) ->
                project v
                    |> map (Tuple.pair k)
            )
        |> map Dict.fromList


{-| -}
sequenceDict : Dict comparable v -> Rec v b (Dict comparable b)
sequenceDict =
    traverseDict recurse


{-| -}
traverseMaybe : (x -> Rec a b c) -> Maybe x -> Rec a b (Maybe c)
traverseMaybe f maybe =
    case maybe of
        Nothing ->
            base Nothing

        Just x ->
            f x |> map Just


{-| -}
sequenceMaybe : Maybe a -> Rec a b (Maybe b)
sequenceMaybe =
    traverseMaybe recurse


{-| -}
traverseResult : (value -> Rec a b c) -> Result error value -> Rec a b (Result error c)
traverseResult f result =
    case result of
        Err e ->
            base (Err e)

        Ok v ->
            f v |> map Ok


{-| -}
sequenceResult : Result error value -> Rec value b (Result error b)
sequenceResult =
    traverseResult recurse


{-| -}
traverseArray : (x -> Rec a b c) -> Array x -> Rec a b (Array c)
traverseArray f =
    Array.toList >> traverseList f >> map Array.fromList


{-| -}
sequenceArray : Array a -> Rec a b (Array b)
sequenceArray =
    traverseArray recurse