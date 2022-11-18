module Recursion.Pipeline.Fold exposing
    ( foldListPipe, foldMapListPipe
    , foldDictPipe, foldMapDictPipe
    , foldArrayPipe, foldMapArrayPipe
    )

{-|


# List

@docs foldListPipe, foldMapListPipe


# Dict

@docs foldDictPipe, foldMapDictPipe


# Array

@docs foldArrayPipe, foldMapArrayPipe

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Recursion exposing (..)
import Recursion.Fold exposing (..)
import Recursion.Pipeline exposing (..)


{-| Pipeable version of [`foldListThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Fold#foldListThen).
-}
foldListPipe : (t -> b -> b) -> b -> List r -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldListPipe fold accum items =
    toPipeable (foldListThen fold accum items)


{-| Pipeable version of [`foldMapListThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Fold#foldMapListThen).
-}
foldMapListPipe : (x -> b -> Rec r t b) -> b -> List x -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldMapListPipe foldMap accum items =
    toPipeable (foldMapListThen foldMap accum items)


{-| Pipeable version of [`foldDictThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Fold#foldDictThen).
-}
foldDictPipe : (comparable -> t -> b -> b) -> b -> Dict comparable r -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldDictPipe fold init dict =
    toPipeable (foldDictThen fold init dict)


{-| Pipeable version of [`foldMapDictThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Fold#foldMapDictThen).
-}
foldMapDictPipe : (comparable -> v -> b -> Rec r t b) -> b -> Dict comparable v -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldMapDictPipe foldMap init dict =
    toPipeable (foldMapDictThen foldMap init dict)


{-| Pipeable version of [`foldArrayThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Fold#foldArrayThen).
-}
foldArrayPipe : (t -> b -> b) -> b -> Array r -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldArrayPipe fold accum items =
    toPipeable (foldArrayThen fold accum items)


{-| Pipeable version of [`foldMapArrayThen`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Fold#foldMapArrayThen).
-}
foldMapArrayPipe : (x -> b -> Rec r t b) -> b -> Array x -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldMapArrayPipe foldMap accum items =
    toPipeable (foldMapArrayThen foldMap accum items)
