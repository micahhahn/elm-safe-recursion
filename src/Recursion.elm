module Recursion exposing
    ( base, recurse, map, andThen
    , runRecursion
    , Rec, recurseThen
    )

{-| This module provides an abstraction over general recursion that allows the recursive computation
to be executed without risk of blowing the stack.

@docs Step

@docs base, recurse, map, andThen

@docs runRecursion

-}


{-| An opaque type representing a step of a recursive computation.

You can construct `Step` values using `base` and `recurse`, and can combine them using `andThen`.

-}
type Rec a b c
    = Base c
    | Recurse a (b -> Rec a b c)


{-| The base case of a recursion.
-}
base : c -> Rec a b c
base =
    Base


{-| Recurse on a value.
-}
recurse : a -> Rec a b b
recurse a =
    Recurse a base


{-| Recurse on a value and specify an action to do immediately after.

`recurseThen x (\f -> ...)` is faster than `recurse x |> andThen (\f -> ...)`

-}
recurseThen : a -> (b -> Rec a b c) -> Rec a b c
recurseThen =
    Recurse


{-| Map on the value.
-}
map : (c -> d) -> Rec a b c -> Rec a b d
map f step =
    case step of
        Base c ->
            Base (f c)

        Recurse a after ->
            Recurse a (after >> map f)


{-| Create a new step to run when another step has finished.
-}
andThen : (c -> Rec a b d) -> Rec a b c -> Rec a b d
andThen next step =
    case step of
        Base c ->
            next c

        Recurse a after ->
            Recurse a (after >> andThen next)


{-| Run a recursion
-}
runRecursion : (a -> Rec a b b) -> a -> b
runRecursion project init =
    let
        go step stack =
            case step of
                Base b ->
                    case stack of
                        [] ->
                            b

                        next :: rest ->
                            go (next b) rest

                Recurse a after ->
                    go (project a) (after :: stack)
    in
    go (project init) []
