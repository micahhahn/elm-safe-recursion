module Recursion.Pipeline.Traverse exposing
    ( sequenceListPipe, traverseListPipe
    , sequenceDictPipe, traverseDictPipe
    , sequenceArrayPipe, traverseArrayPipe
    , sequenceMaybePipe, traverseMaybePipe
    , sequenceResultPipe, traverseResultPipe
    )

{-| This module provides pipeable versions of the `sequence____Then` and `traverse____Then` functions from
[`Recursion.Traverse`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse).


# List

@docs sequenceListPipe, traverseListPipe


# Dict

@docs sequenceDictPipe, traverseDictPipe


# Array

@docs sequenceArrayPipe, traverseArrayPipe


# Maybe

@docs sequenceMaybePipe, traverseMaybePipe


# Result

@docs sequenceResultPipe, traverseResultPipe

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Recursion exposing (..)
import Recursion.Pipeline exposing (..)
import Recursion.Traverse exposing (..)


{-| Pipeable version of [`sequenceListThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse#sequenceListThen).
-}
sequenceListPipe :
    List r
    -> Pipeline (List t -> z) (Rec r t a)
    -> Pipeline z (Rec r t a)
sequenceListPipe items =
    toPipeable (sequenceListThen items)


{-| Pipeable version of [`traverseListThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse#traverseListThen).
-}
traverseListPipe :
    (x -> Rec r t a)
    -> List x
    -> Pipeline (List a -> z) (Rec r t b)
    -> Pipeline z (Rec r t b)
traverseListPipe project items =
    toPipeable (traverseListThen project items)


{-| Pipeable version of [`sequenceDictThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse#sequenceDictThen).
-}
sequenceDictPipe :
    Dict comparable r
    -> Pipeline (Dict comparable t -> z) (Rec r t a)
    -> Pipeline z (Rec r t a)
sequenceDictPipe dict =
    toPipeable (sequenceDictThen dict)


{-| Pipeable version of [`traverseDictThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse#traverseDictThen).
-}
traverseDictPipe :
    (comparable -> v -> Rec r t a)
    -> Dict comparable v
    -> Pipeline (Dict comparable a -> z) (Rec r t b)
    -> Pipeline z (Rec r t b)
traverseDictPipe project dict =
    toPipeable (traverseDictThen project dict)


{-| Pipeable version of [`sequenceArrayThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse#sequenceArrayThen).
-}
sequenceArrayPipe :
    Array r
    -> Pipeline (Array t -> z) (Rec r t a)
    -> Pipeline z (Rec r t a)
sequenceArrayPipe items =
    toPipeable (sequenceArrayThen items)


{-| Pipeable version of [`traverseArrayThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse#traverseArrayThen).
-}
traverseArrayPipe :
    (x -> Rec r t a)
    -> Array x
    -> Pipeline (Array a -> z) (Rec r t b)
    -> Pipeline z (Rec r t b)
traverseArrayPipe project items =
    toPipeable (traverseArrayThen project items)


{-| Pipeable version of [`sequenceMaybeThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse#sequenceMaybeThen).
-}
sequenceMaybePipe :
    Maybe r
    -> Pipeline (Maybe t -> z) (Rec r t a)
    -> Pipeline z (Rec r t a)
sequenceMaybePipe maybe =
    toPipeable (sequenceMaybeThen maybe)


{-| Pipeable version of [`traverseMaybeThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse#traverseMaybeThen).
-}
traverseMaybePipe :
    (x -> Rec r t a)
    -> Maybe x
    -> Pipeline (Maybe a -> z) (Rec r t b)
    -> Pipeline z (Rec r t b)
traverseMaybePipe project maybe =
    toPipeable (traverseMaybeThen project maybe)


{-| Pipeable version of [`sequenceResultThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse#sequenceResultThen).
-}
sequenceResultPipe :
    Result e r
    -> Pipeline (Result e t -> z) (Rec r t a)
    -> Pipeline z (Rec r t a)
sequenceResultPipe result =
    toPipeable (sequenceResultThen result)


{-| Pipeable version of [`traverseResultThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse#traverseResultThen).
-}
traverseResultPipe :
    (v -> Rec r t a)
    -> Result e v
    -> Pipeline (Result e a -> z) (Rec r t b)
    -> Pipeline z (Rec r t b)
traverseResultPipe project result =
    toPipeable (traverseResultThen project result)
