module Recursion.Fold exposing (..)

import Recursion exposing (..)


foldlList : (c -> ( Step a b, b -> d -> d )) -> d -> List c -> (d -> Step a b) -> Step a b
foldlList project init items after =
    let
        go : List c -> d -> Step a b
        go todo accum =
            case todo of
                [] ->
                    after accum

                item :: rest ->
                    let
                        ( step, fold ) =
                            project item
                    in
                    step
                        |> andThen (\b -> go rest (fold b accum))
    in
    go items init
