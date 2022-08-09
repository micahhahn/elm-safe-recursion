module Recursion exposing (Step, andThen, base, recurse, runRecursion)


type Step a b
    = Base b
    | Recurse a (List (b -> Step a b))


base : b -> Step a b
base =
    Base


recurse : a -> Step a b
recurse a =
    Recurse a []


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
