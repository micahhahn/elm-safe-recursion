module Recursion.Pipeline.Traverse exposing (sequenceArrayPipe, sequenceDictPipe, sequenceListPipe, sequenceMaybePipe, sequenceResultPipe, traverseArrayPipe, traverseDictPipe, traverseListPipe, traverseMaybePipe, traverseResultPipe)

import Array exposing (Array)
import Dict exposing (Dict)
import Recursion exposing (..)
import Recursion.Pipeline exposing (..)
import Recursion.Traverse exposing (..)


sequenceListPipe : List r -> Pipeline (List t -> z) (Rec r t a) -> Pipeline z (Rec r t a)
sequenceListPipe items =
    toPipeable (sequenceListThen items)


traverseListPipe : (x -> Rec r t a) -> List x -> Pipeline (List a -> z) (Rec r t b) -> Pipeline z (Rec r t b)
traverseListPipe project items =
    toPipeable (traverseListThen project items)


sequenceDictPipe : Dict comparable r -> Pipeline (Dict comparable t -> z) (Rec r t a) -> Pipeline z (Rec r t a)
sequenceDictPipe dict =
    toPipeable (sequenceDictThen dict)


traverseDictPipe : (comparable -> v -> Rec r t a) -> Dict comparable v -> Pipeline (Dict comparable a -> z) (Rec r t b) -> Pipeline z (Rec r t b)
traverseDictPipe project dict =
    toPipeable (traverseDictThen project dict)


sequenceArrayPipe : Array r -> Pipeline (Array t -> z) (Rec r t a) -> Pipeline z (Rec r t a)
sequenceArrayPipe items =
    toPipeable (sequenceArrayThen items)


traverseArrayPipe : (x -> Rec r t a) -> Array x -> Pipeline (Array a -> z) (Rec r t b) -> Pipeline z (Rec r t b)
traverseArrayPipe project items =
    toPipeable (traverseArrayThen project items)


sequenceMaybePipe : Maybe r -> Pipeline (Maybe t -> z) (Rec r t a) -> Pipeline z (Rec r t a)
sequenceMaybePipe maybe =
    toPipeable (sequenceMaybeThen maybe)


traverseMaybePipe : (x -> Rec r t a) -> Maybe x -> Pipeline (Maybe a -> z) (Rec r t b) -> Pipeline z (Rec r t b)
traverseMaybePipe project maybe =
    toPipeable (traverseMaybeThen project maybe)


sequenceResultPipe : Result e r -> Pipeline (Result e t -> z) (Rec r t a) -> Pipeline z (Rec r t a)
sequenceResultPipe result =
    toPipeable (sequenceResultThen result)


traverseResultPipe : (v -> Rec r t a) -> Result e v -> Pipeline (Result e a -> z) (Rec r t b) -> Pipeline z (Rec r t b)
traverseResultPipe project result =
    toPipeable (traverseResultThen project result)
