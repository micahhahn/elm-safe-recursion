module Recursion exposing
    ( Rec
    , base, recurse
    , runRecursion
    , recurse2, recurse3, recurse4
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

@docs base, recurse, recurseThen


## Manipulating a `Rec`

@docs map, andThen

Check out [`Recursion.Traverse`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Traverse)
and [`Recursion.Fold`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/2.0.0/Recursion-Fold)
for helpers that work with containers of recursive types.


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
                                recurseThen r
                                    (\newR ->
                                        baes <| Node newL newR
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

-}
type Rec r t
    = Base t
    | Recurse r (t -> Rec r t)


{-| The base case of a recursion. The value is injected directly into the `Rec` type.
-}
base : t -> Rec r t
base =
    Base


{-| Recurse on a value.

When the recursion is complete the `Rec` will contain a value of type `t`.

-}
recurse : r -> (t -> Rec r t) -> Rec r t
recurse =
    Recurse


{-| Recurse over 2 values
-}
recurse2 : r -> r -> (t -> t -> Rec r t) -> Rec r t
recurse2 r1 r2 f =
    Recurse r1 (\t1 -> Recurse r2 (\t2 -> f t1 t2))


recurse3 : r -> r -> r -> (t -> t -> t -> Rec r t) -> Rec r t
recurse3 r1 r2 r3 f =
    Recurse r1 (\t1 -> Recurse r2 (\t2 -> Recurse r3 (\t3 -> f t1 t2 t3)))


recurse4 : r -> r -> r -> r -> (t -> t -> t -> t -> Rec r t) -> Rec r t
recurse4 r1 r2 r3 r4 f =
    Recurse r1 (\t1 -> Recurse r2 (\t2 -> Recurse r3 (\t3 -> Recurse r4 (\t4 -> f t1 t2 t3 t4))))


{-| Run a recursion given a function to run one layer and an initial value.
-}
runRecursion : (r -> Rec r t) -> r -> t
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
