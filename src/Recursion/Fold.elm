module Recursion.Fold exposing
    ( foldlList, foldrList
    , foldlDict, foldrDict
    , foldlArray, foldrArray
    )

{-|


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


{-| -}
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


{-| -}
foldrList : (x -> Rec a b c) -> (c -> d -> d) -> d -> List x -> Rec a b d
foldrList project fold init list =
    foldlList project fold init (List.reverse list)


uncurry f ( a, b ) =
    f a b


{-| -}
foldlDict : (comparable -> v -> Rec a b c) -> (c -> d -> d) -> d -> Dict comparable v -> Rec a b d
foldlDict project fold init =
    Dict.toList >> foldlList (uncurry project) fold init


{-| -}
foldrDict : (comparable -> v -> Rec a b c) -> (c -> d -> d) -> d -> Dict comparable v -> Rec a b d
foldrDict project fold init =
    Dict.toList >> foldrList (uncurry project) fold init


{-| -}
foldlArray : (x -> Rec a b c) -> (c -> d -> d) -> d -> Array x -> Rec a b d
foldlArray project fold init =
    Array.toList >> foldlList project fold init


{-| -}
foldrArray : (x -> Rec a b c) -> (c -> d -> d) -> d -> Array x -> Rec a b d
foldrArray project fold init =
    Array.toList >> foldrList project fold init
