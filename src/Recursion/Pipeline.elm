module Recursion.Pipeline exposing (Pipeline, basePipe, begin, end, recursePipe, toPipeable)

import Recursion exposing (..)


type alias Pipeline a r =
    (a -> r) -> r


{-| Begin a pipeline.
-}
begin : a -> Pipeline a r
begin =
    (|>)


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
end : Pipeline a (Rec r t a) -> Rec r t a
end =
    (|>) base
