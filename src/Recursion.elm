module Recursion exposing
    ( Step
    , base, recurse, map, andThen
    , runRecursion
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
type Step a b c
    = Base c
    | Recurse a (b -> Step a b c)


{-| The base case of a recursion.
-}
base : c -> Step a b c
base =
    Base


{-| Recurse on a value.
-}
recurse : a -> Step a b b
recurse a =
    Recurse a base


{-| Map on the value.
-}
map : (c -> d) -> Step a b c -> Step a b d
map f step =
    case step of
        Base c ->
            Base (f c)

        Recurse a after ->
            Recurse a (after >> map f)


{-| Create a new step to run when another step has finished.
-}
andThen : (c -> Step a b d) -> Step a b c -> Step a b d
andThen next step =
    case step of
        Base c ->
            next c

        Recurse a after ->
            Recurse a (after >> andThen next)


{-| Run a recursion
-}
runRecursion : (a -> Step a b b) -> a -> b
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
