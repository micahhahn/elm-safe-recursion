module Recursion.Pipeline.Fold exposing (foldArrayPipe, foldDictPipe, foldListPipe, foldMapArrayPipe, foldMapDictPipe, foldMapListPipe)

import Array exposing (Array)
import Dict exposing (Dict)
import Recursion exposing (..)
import Recursion.Fold exposing (..)
import Recursion.Pipeline exposing (..)


foldListPipe : (t -> b -> b) -> b -> List r -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldListPipe fold accum items =
    toPipeable (foldListThen fold accum items)


foldMapListPipe : (x -> b -> Rec r t b) -> b -> List x -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldMapListPipe foldMap accum items =
    toPipeable (foldMapListThen foldMap accum items)


foldDictPipe : (comparable -> t -> b -> b) -> b -> Dict comparable r -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldDictPipe fold init dict =
    toPipeable (foldDictThen fold init dict)


foldMapDictPipe : (comparable -> v -> b -> Rec r t b) -> b -> Dict comparable v -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldMapDictPipe foldMap init dict =
    toPipeable (foldMapDictThen foldMap init dict)


foldArrayPipe : (t -> b -> b) -> b -> Array r -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldArrayPipe fold accum items =
    toPipeable (foldArrayThen fold accum items)


foldMapArrayPipe : (x -> b -> Rec r t b) -> b -> Array x -> Pipeline (b -> z) (Rec r t a) -> Pipeline z (Rec r t a)
foldMapArrayPipe foldMap accum items =
    toPipeable (foldMapArrayThen foldMap accum items)
