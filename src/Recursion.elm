module Recursion exposing (Trampoline(..))


type Trampoline a
    = Return a
    | Suspend (() -> Trampoline a)
    | Bind
