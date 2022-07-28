# Safe and elegant recursion in Elm

This package provides tools to use recursion safely in elm without sacrificing the elegance of general recursion. 

## Example

Imagine we have a generic tree type that we want to write a `map` function for: 

```elm
type Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
```

The safe solution to implement a map function using this library would look like the following:

```elm
mapTree : (a -> b) -> Tree a -> Tree b
mapTree f initTree = 
    runRecursion (\tree -> 
        case tree of
            Leaf a -> 
                base <| Leaf (f a)

            Node l r -> 
                recurse l |> andThen (\newL -> recurse r |> andThen (\newR -> base <| Node newL newR))
    ) initTree
```

For more on the types and functions involved and details on how this works check out the [`Recursion`](https://package.elm-lang.org/packages/micahhahn/elm-safe-recursion/1.0.1/Recursion/) module.

This module pairs extremely well with the elm-review rule [`NoUnoptimizedRecursion`](https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion).

## Can't I just...

### Refactor my recursive code by hand to be Tail Call Optimized? 

Sure! But for non-trivial recursions like the `mapTree` example above or recursive data structures involving lists you'll find yourself needing to write parallel data structures just to keep track of where you are in a step of the recursion.  You quickly lose the elegance of recursion in all the bookkeeping. 

### Use [elm-tail-recursion](https://package.elm-lang.org/packages/joneshf/elm-tail-recursion/latest/TailRecursion)?

Sure! But again for non-trivial recursions you will require custom data structures and are only in a slightly better situation than writing tail call optimized code by hand.

### Use [trampoline](https://package.elm-lang.org/packages/elm-lang/trampoline/latest/Trampoline)?

Sure! But it's the same situation as above.

## Additional Information 

This library is based on the excellent paper "Stackless Scala With Free Monads" by Bjarnason. Unfortunately, we don't have existentially quantified types in Elm so we can not directly implement the `Trampoline` monad from the paper as a dataype. 

So instead we borrow some inspiration from [recent work in PureScript](https://github.com/purescript-contrib/purescript-parsing/pull/154) and use Continuation Passing Style to acheive monadic behavior in the `Rec` type. 