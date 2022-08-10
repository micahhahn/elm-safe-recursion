module Recursion exposing
    ( Step
    , base, recurse, andThen
    , runRecursion
    )

{-| This module provides an abstraction over general recursion that allows the recursive computation
to be executed without risk of blowing the stack.

@docs Step

@docs base, recurse, andThen

@docs runRecursion

-}


{-| An opaque type representing a step of a recursive computation.

You can construct `Step` values using `base` and `recurse`, and can combine them using `andThen`.

-}
type Step a b
    = Base b
    | Recurse a (List (b -> Step a b))


{-| The base case of a recursion.
-}
base : b -> Step a b
base =
    Base


{-| Recurse on a value.
-}
recurse : a -> Step a b
recurse a =
    Recurse a []


{-| Create a new step to run when another step has finished.

Note that the type is slightly different from other well known `andThen` functions like [Maybe.andThen](https://package.elm-lang.org/packages/elm/core/latest/Maybe#andThen) or [Result.andThen](https://package.elm-lang.org/packages/elm/core/latest/Result#andThen) becuase
we are **not** able to change the type of `b`.

-}
andThen : (b -> Step a b) -> Step a b -> Step a b
andThen next step =
    case step of
        Base b ->
            next b

        Recurse a queue ->
            Recurse a (next :: queue)


{-| Assumes queue is stored in reverse order (last elment stored in head)
-}
queueStackMerge : List a -> List a -> List a
queueStackMerge queue stack =
    case queue of
        [] ->
            stack

        item :: rest ->
            queueStackMerge rest (item :: stack)


{-| Run a recursion
-}
runRecursion : (a -> Step a b) -> a -> b
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

                Recurse a queue ->
                    go (project a) (queueStackMerge queue stack)
    in
    go (project init) []
