module Recursion.Pipeline exposing
    ( startPipe, endPipe
    , basePipe, recursePipe
    , toPipeable, Pipeline
    )

{-|

@docs startPipe, endPipe

@docs basePipe, recursePipe

@docs toPipeable, Pipeline

-}

import Recursion exposing (..)


{-| A type that encapuslates a recursion pipeline.
-}
type alias Pipeline a r =
    (a -> r) -> r


{-| Begin a pipeline.
-}
startPipe : a -> Pipeline a r
startPipe =
    (|>)


{-| Create your own pipelineable function
-}
toPipeable : ((a -> r) -> r) -> Pipeline (a -> z) r -> Pipeline z r
toPipeable thenFunc before cont =
    before ((<<) cont >> thenFunc)


{-| A pipeable version of `base`
-}
basePipe : x -> Pipeline (x -> z) (Rec r t a) -> Pipeline z (Rec r t a)
basePipe x =
    toPipeable ((|>) x)


{-| A pipeable version of `recurse`
-}
recursePipe : r -> Pipeline (t -> z) (Rec r t a) -> Pipeline z (Rec r t a)
recursePipe r =
    toPipeable (recurseThen r)


{-| End a pipeline, recovering the `Rec` object.
-}
endPipe : Pipeline a (Rec r t a) -> Rec r t a
endPipe =
    (|>) base
