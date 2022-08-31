module Recursion exposing
    ( Rec
    , base, recurse, recurseThen
    , map, andThen
    , runRecursion
    )

{-| This module provides an abstraction over general recursion that allows the recursive computation
to be executed without risk of blowing the stack.

If you are unfamiliar with why we need to be careful about unsafe recursion in Elm,
[this article](https://functional-programming-in-elm.netlify.app/recursion/tail-call-elimination.html) describes the Tail-Call Elimination very well.

It is not terribly difficult to rewrite simple recursions to be safe by taking advantage of the Tail Call Optimization.
However, the moment you need to recurse on two or more elements in a data structure it becomes quite hairy to write safely
and the resulting code loses much of the beauty of recursion.

This module presents ways to create and execute the `Rec` monad, which is sufficiently powerful to represent non-simple recursion safely **and** will allow
you to preserve the recursive elegance that makes functional programming beautiful.


## Core Type

@docs Rec


## Creating a `Rec`

@docs base, recurse, recurseMap, recurseThen


## Manipulating a `Rec`

@docs map, andThen

Check out [`Recursion.Traverse`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/1.0.1/Recursion-Traverse)
and [`Recursion.Fold`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/1.0.1/Recursion-Fold)
for helpers that work with contains of recursive types.


## Running a `Rec`

@docs runRecursion


# Example

Imagine we have a generic binary tree type that we want to write a map function for:

    type Tree a
        = Leaf a
        | Node (Tree a) (Tree a)

The standard recursive map algorithm is straightforward:

    mapTree : (a -> b) -> Tree a -> Tree b
    mapTree f tree =
        case tree of
            Leaf a ->
                Leaf (f a)

            Node l r ->
                Node (mapTree f l) (mapTree f r)

⚠️⚠️⚠️ This is unsafe! ⚠️⚠️⚠️

Since the recursive calls to `mapTree` are not located in tail call position the Tail Call Optimization will not fire.
We are exposing ourselves to a crash if the tree is deep enough that we would have a stack overflow while executing it!


## Using elm-safe-recursion

    mapTree : (a -> b) -> Tree a -> Tree b
    mapTree f initTree =
        runRecursion
            (\tree ->
                case tree of
                    Leaf a ->
                        base (Leaf (f a))

                    Node l r ->
                        recurseThen l
                            (\newL ->
                                recurseMap r
                                    (\newR ->
                                        Node newL newR
                                    )
                            )
            )
            initTree

-}


{-| An opaque type representing a recursive computation.

  - `r` is the **r**ecursive type.
  - `t` is the **t**arget type that we are converting to.
  - `a` is a type for intermediate parts of the computation.

I think it is helpful to think of `Rec` like the `Promise` type in javascript. Simliar to a `Promise`, the
result in a `Rec` value might not be available yet because it needs to recursively evaluated in a separate step.
So instead of directly manipulating the value in a `Rec`, we instead can specify actions to be done with the value
when it is available using `map` and `andThen`.

`Rec` is a monad over type `a`.

-}
type Rec r t a
    = Base a
    | Recurse r (t -> Rec r t a)


{-| The base case of a recursion. The value is injected directly into the `Rec` type.
-}
base : a -> Rec r t a
base =
    Base


{-| Recurse on a value.

When the recursion is complete this will contain a value of type `t`.

-}
recurse : r -> Rec r t t
recurse r =
    Recurse r base

{-| Recurse on a value and then take another action on the result.

If you find yourself writing code that looks like `recurse x |> andThen` you should
consider using `recurseThen` instead as it will be much more efficient.

-}
recurseThen : r -> (t -> Rec r t a) -> Rec r t a
recurseThen =
    Recurse


{-| Apply a function to the result of a `Rec` computation.
-}
map : (a -> b) -> Rec r t a -> Rec r t b
map f step =
    case step of
        Base t ->
            Base (f t)

        Recurse r after ->
            Recurse r (after >> map f)


{-| Apply a function to the result of a `Rec` computation that can specify more recursion.
-}
andThen : (a -> Rec r t b) -> Rec r t a -> Rec r t b
andThen next step =
    case step of
        Base t ->
            next t

        Recurse r after ->
            Recurse r (after >> andThen next)


{-| Run a recursion given a function to run one layer and an initial value.
-}
runRecursion : (r -> Rec r t t) -> r -> t
runRecursion project init =
    let
        go step stack =
            case step of
                Base t ->
                    case stack of
                        [] ->
                            t

                        next :: rest ->
                            go (next t) rest

                Recurse r after ->
                    go (project r) (after :: stack)
    in
    go (project init) []
