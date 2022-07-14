module Recursion exposing (andThen, base, recurse, runRecursion)


type Recursion a b
    = Base b
    | Recurse a
    | Bind (Recursion a b) (b -> Recursion a b)


base : b -> Recursion a b
base =
    Base


recurse : a -> Recursion a b
recurse =
    Recurse


andThen : (b -> Recursion a b) -> Recursion a b -> Recursion a b
andThen func recursion =
    Bind recursion func



-- A stack might be a better evaluation strategy here than the Bind shuffling


runRecursion : (a -> Recursion a b) -> a -> b
runRecursion f =
    let
        go : List (b -> Recursion a b) -> Recursion a b -> b
        go bindings rec =
            case rec of
                Base b ->
                    case bindings of
                        [] ->
                            b

                        binding :: rest ->
                            go rest (binding b)

                Recurse a ->
                    go bindings (f a)

                Bind innerRec binding ->
                    go (binding :: bindings) innerRec
    in
    f >> go []
