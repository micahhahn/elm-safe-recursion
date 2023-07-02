module RecursionTest exposing (suite)

import Expect
import Recursion exposing (..)
import Test exposing (..)


slowSum : Int -> Float
slowSum =
    runRecursion
        (\i ->
            case i of
                0 ->
                    base 0.0

                _ ->
                    recurse (i - 1) (\x -> base (x + toFloat i))
        )


safetyTests : Test
safetyTests =
    describe "Safety Tests"
        [ test "slowSum1 doesn't stack overflow" <|
            \_ ->
                slowSum 100000 |> Expect.within (Expect.Absolute 0) ((100000.0 * 100001.0) / 2.0)
        ]


suite : Test
suite =
    describe "Recursion"
        [ safetyTests
        ]
