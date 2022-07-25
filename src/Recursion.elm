module Recursion exposing
    ( runRecursion, Rec
    , base
    , recurse
    , map
    , andThen
    )

{-| This module provides functions to construct the `Rec` type which encapsulates a recursion computation,
and `runRecursion` to execute the recursion on a value. Recursions that are constructed and executed in this way
are safe from blowing up the callstack.

If you are unfamiliar with why we need to be careful about unsafe recursion in Elm,
[this is a good resource written by Evan describing the Tail-Call Elimination](https://functional-programming-in-elm.netlify.app/recursion/tail-call-elimination.html)

It is not terribly difficult to rewrite simple recursions to be safe by taking advantage of the Tail Call Optimization.
However, the moment you need to recurse on two or more elements in a data structure it becomes quite hairy to write safely
and the resulting code loses much of the beauty of recursion.

The `Rec` type is sufficently powerful to be able to represent non-simple recursion safely **and** will allow you to keep the recursive elegance that makes functional programming beautiful.


## Example

Imagine we have a generic binary tree type that we want to write a `map` function for:

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

⚠️ This is unsafe! ⚠️

Since the recursive calls to `mapTree` are not located in tail call position the Tail Call Optimization will not fire.
We are exposing ourselves to a crash if the tree is deep enough that we would have a stack overflow while executing it!


## Example using this library

    mapTree : (a -> b) -> Tree a -> Tree b
    mapTree f initTree =
        runRecursion
            (\tree ->
                case tree of
                    Leaf a ->
                        base (Leaf (f a))

                    Node l r ->
                        recurse l
                            |> andThen
                                (\newL ->
                                    recurse r
                                        |> map (\newR -> Node newL newR)
                                )
            )
            initTree

This looks roughly the same as the naive implementation,
but we definitely have some additional complexity here so let's break it down step by step!

The first thing to notice is that there are no explicit calls to `mapTree` in this example.
Instead of recursively calling `mapTree` on the `l` and `r` values of the `Node` we instead call `recurse`.

`recurse` produces the opaque `Rec` type so we cannot simply just pass `recurse l` and `recurse r` into the `Node` constructor.
Instead we stitch them together using `andThen` and `map` combinators which give us access to the resulting `newL` and `newR` values.

We also explicitly call `base` on `Leaf (f a)` to terminate the recursion in the base case.

Finally `runRecursion` takes this function we provide and iteratively runs it in a stack safe way to produce the final value.


## Recursion

@docs runRecursion, Rec

@docs base
@docs recurse
@docs map
@docs andThen


## Containers

If you data type you are recursing uses a container that holds a recursive value, you'll want to check out `Recursion.Traverse` and `Recursion.Fold`.

`Recursion.Traverse` is useful for cases where we want to **preserve** the structure of our datatype.

`Recursion.Fold` is useful for cases where we want to **collapse** the structure of our datatype into another type.

-}


{-| A type that encapsulates recursion computation.

  - `a` is the original type of the object we are recursing over.

  - `b` is the final type that we are constructing (Note that a `b` is the final result that `runRecursion` produces).

  - `c` is an intermediate type used to help compose recursions together.

I think it is helpful to think of `Rec` as similar to the `Promise` type in javascript. Similar to a `Promise`, a `Rec` will sometimes not have a value at hand.
So instead of using the value directly, we instead use `map` and `andThen` to specify actions to do when the result is finally available.

`Rec a b c` forms a Functor over `c` with the `base` and `map` functions. It also forms a monad over `c` with the `base` and `andThen` functions.

-}
type Rec a b c
    = Rec ((a -> (b -> Rec a b c) -> Step a b) -> (c -> Step a b) -> Step a b)


{-| The "base case" of recursion. We simply inject a result value straight into the `Rec` type.
-}
base : c -> Rec a b c
base b =
    Rec (\_ done -> done b)


{-| Specifies a recursion over the passed value. The function passed to `runRecursion` will eventually be run with this value.
-}
recurse : a -> Rec a b b
recurse a =
    Rec (\more _ -> more a base)


runRec : Rec a b c -> ((a -> (b -> Rec a b c) -> Step a b) -> (c -> Step a b) -> Step a b)
runRec (Rec c) =
    c


{-| Map over the result of a `Rec` once it is complete.

You should prefer to use `map` instead of `andThen` whenever possible as it is slighly faster and easier on the eyes.

For example if you have code that looks like this:

    |> andThen (\x -> base <| ... )

You can and should rewrite it to this:

    |> map (\x -> ...)

-}
map : (c -> d) -> Rec a b c -> Rec a b d
map f (Rec cont) =
    Rec
        (\more done ->
            cont (\a unwind -> more a (unwind >> map f)) (f >> done)
        )


{-| Chain together one `Rec` type and potentially produce another.

Prefer to use `map` instead of `andThen` if possible.

-}
andThen : (c -> Rec a b d) -> Rec a b c -> Rec a b d
andThen f (Rec cont) =
    Rec
        (\more done ->
            cont (\a unwind -> more a (unwind >> andThen f)) (\c -> runRec (f c) more done)
        )


type Step a b
    = Loop a (b -> Rec a b b)
    | Done b


{-| Iteratively calls the passed function to run one layer of a recursion at a time without growing the callstack.
-}
runRecursion : (a -> Rec a b b) -> a -> b
runRecursion f init =
    let
        go : Rec a b b -> List (b -> Rec a b b) -> b
        go (Rec cont) stack =
            case cont Loop Done of
                Loop next newStack ->
                    go (f next) (newStack :: stack)

                Done b ->
                    case stack of
                        unwind :: rest ->
                            go (unwind b) rest

                        _ ->
                            b
    in
    go (f init) []
