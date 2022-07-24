module Recursion exposing (Rec, andThen, base, map, recurse, runRecursion, traverseList)


type Rec a b c
    = Rec ((a -> (b -> Rec a b c) -> Step a b) -> (c -> Step a b) -> Step a b)


base : c -> Rec a b c
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
    Rec
        (\more done ->
            cont (\a unwind -> more a (unwind >> map f)) (f >> done)
        )


andThen : (c -> Rec a b d) -> Rec a b c -> Rec a b d
andThen f (Rec cont) =
    Rec
        (\more done ->
            cont (\a unwind -> more a (unwind >> andThen f)) (\c -> runRec (f c) more done)
        )


traverseList : (x -> Rec a b c) -> List x -> Rec a b (List c)
traverseList f list =
    let
        go todos done =
            case todos of
                item :: rest ->
                    f item |> andThen (\newItem -> go rest (newItem :: done))

                _ ->
                    base done
    in
    go (List.reverse list) []


sequenceList : List a -> Rec a b (List b)
sequenceList =
    traverseList recurse


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
