module Custom exposing (..)


type Rec a r b
    = Rec ((a -> r) -> (b -> r) -> r)


base : b -> Rec a r b
base b =
    Rec (\_ done -> done b)


recurse : a -> Rec a r b
recurse a =
    Rec (\more _ -> more a)


runRec : Rec a r b -> ((a -> r) -> (b -> r) -> r)
runRec (Rec c) =
    c


bind : Rec a r b -> (b -> Rec a r c) -> Rec a r c
bind (Rec c) f =
    Rec (\more done -> c more (\b -> runRec (f b) more done))


andThen : (b -> Rec a r c) -> Rec a r b -> Rec a r c
andThen f a =
    bind a f


type Step a b
    = Loop a
    | Done b


runRecursion : (a -> Rec a (Step a b) b) -> a -> b
runRecursion f a =
    case runRec (f a) Loop Done of
        Done b ->
            b

        Loop x ->
            runRecursion f x
