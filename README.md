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

For more details on the types and functions involved and why this works check out the `Recursion` module.