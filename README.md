# Safe and elegant recursion in Elm

This package provides tools to use recursion safely in elm without sacrificing the elegance of recursive calls. 

This is accomplished by having the user call `runRecursion` passing a function that will run one step of the recursion at a time.  

## Example

Imagine we have a generic tree type that we want to write a `map` function for: 

```elm
type Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
```

The standard recursive map algorithm is straightforward: 

```elm
mapTree : (a -> b) -> Tree a -> Tree b
mapTree f tree = 
    case tree of
        Leaf a -> 
            Leaf (f a)

        Node l r -> 
            Node (mapTree f l) (mapTree f r)
```

⚠️⚠️⚠️ This is unsafe! ⚠️⚠️⚠️ 

Since the recursive calls to `mapTree` are not located in tail call position the Tail Call Optimization will not fire. We are exposing ourselves to a crash if the tree is deep enough that we would have a stack overflow while executing it! 

### A better way

Let's see what the implementation of `mapTree` would be using `elm-safe-recursion`: 

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

If you squint this roughly looks like the straightforward recursive algorithm with the "base" and "recursive" cases called out explicitly with a little bit of glue. 

Let's break down how this works piece by piece.