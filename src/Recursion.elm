module Recursion exposing (Rec, andThen, base, map, recurse, runRecursion)


type Rec a b c
    = Rec ((a -> (b -> Rec a b c) -> Step a b) -> (c -> Step a b) -> Step a b)


base : b -> Rec a b b
base b =
    Rec (\_ done -> done b)


recurse : a -> Rec a b b
recurse a =
    Rec (\more _ -> more a base)


runRec : Rec a b c -> ((a -> (b -> Rec a b c) -> Step a b) -> (c -> Step a b) -> Step a b)
runRec (Rec c) =
    c


map : (c -> d) -> Rec a b c -> Rec a b d
map f (Rec cont) =
    Rec (\more done -> cont (\a unwind -> more a (unwind >> map f)) (f >> done))


andThen : (c -> Rec a b d) -> Rec a b c -> Rec a b d
andThen f (Rec cont) =
    Rec (\more done -> cont (\a unwind -> more a (unwind >> andThen f)) (\c -> runRec (f c) more done))


type Step a b
    = Loop a (b -> Rec a b b)
    | Done b


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
