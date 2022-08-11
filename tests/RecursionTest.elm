module RecursionTest exposing (..)

import Expect
import Recursion exposing (..)
import Test exposing (..)


slowSum1 : Int -> Float
slowSum1 =
    runRecursion
        (\i ->
            case i of
                0 ->
                    base 0.0

                _ ->
                    recurse (i - 1) |> map ((+) <| toFloat i)
        )


slowSum2 : Int -> Float
slowSum2 =
    runRecursion
        (\i ->
            case i of
                0 ->
                    base 0.0

                _ ->
                    recurse (i - 1) |> map ((+) <| toFloat i)
        )


slowSum3 : Int -> Float
slowSum3 =
    runRecursion
        (\i ->
            case i of
                0 ->
                    base 0.0

                _ ->
                    recurse (i - 1) |> andThen (\f -> base <| f + toFloat i)
        )


safetyTests : Test
safetyTests =
    describe "Safety Tests"
        [ test "slowSum1 doesn't stack overflow" <|
            \_ ->
                slowSum1 100000 |> Expect.within (Expect.Absolute 0) ((100000.0 * 100001.0) / 2.0)
        , test "slowSum2 doesn't stack overflow" <|
            \_ ->
                slowSum2 100000 |> Expect.within (Expect.Absolute 0) ((100000.0 * 100001.0) / 2.0)
        , test "slowSum3 doesn't stack overflow" <|
            \_ ->
                slowSum3 100000 |> Expect.within (Expect.Absolute 0) ((100000.0 * 100001.0) / 2.0)
        ]


suite : Test
suite =
    describe "Recursion"
        [ safetyTests
        ]
