module Recursion exposing (Step(..), runRecursion)


type Step a b
    = Base b
    | Recurse a (b -> Step a b)


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

                Recurse a next ->
                    go (project a) (next :: stack)
    in
    go (project init) []
