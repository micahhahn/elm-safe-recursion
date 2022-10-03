module Recursion.Types exposing (Rec, RecStep(..))


type RecStep r t a
    = Base a
    | Recurse r (t -> RecStep r t a)


type alias Rec r t a =
    (a -> RecStep r t t) -> RecStep r t t
