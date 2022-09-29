module Recursion.PipelineTest exposing (suite)

import Expect
import Recursion exposing (..)
import Recursion.Pipeline exposing (..)
import Test exposing (..)


fibNaive : Int -> Int
fibNaive x =
    case x of
        0 ->
            1

        1 ->
            1

        _ ->
            fibNaive (x - 1) + fibNaive (x - 2)


fib : Int -> Int
fib =
    runRecursion <|
        \x ->
            case x of
                0 ->
                    base 1

                1 ->
                    base 1

                _ ->
                    startPipe (+)
                        |> recursePipe (x - 1)
                        |> recursePipe (x - 2)
                        |> endPipe


correctnessTests : Test
correctnessTests =
    describe "Correctenss"
        [ test "fib returns same answers as naive algorithm" <|
            \_ ->
                let
                    naive =
                        List.range 1 10 |> List.map fibNaive

                    fibs =
                        List.range 1 10 |> List.map fib
                in
                Expect.equalLists naive fibs
        ]


suite : Test
suite =
    describe "Recursion.Pipeline"
        [ correctnessTests ]
