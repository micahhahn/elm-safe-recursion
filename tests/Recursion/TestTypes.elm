module Recursion.TestTypes exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)


hugeSize : Int
hugeSize =
    10000


type ListTree a
    = ListNode a (List (ListTree a))


hugeListTree : ListTree Int
hugeListTree =
    List.range 1 hugeSize
        |> List.map (\x -> ListNode x [])
        |> ListNode 0


type DictTree a
    = DictNode a (Dict String (DictTree a))


hugeDictTree : DictTree Int
hugeDictTree =
    List.range 1 hugeSize
        |> List.map (\x -> ( String.fromInt x, DictNode x Dict.empty ))
        |> Dict.fromList
        |> DictNode 0


type ArrayTree a
    = ArrayNode a (Array (ArrayTree a))


hugeArrayTree : ArrayTree Int
hugeArrayTree =
    List.range 1 hugeSize
        |> List.map (\x -> ArrayNode x (Array.fromList []))
        |> Array.fromList
        |> ArrayNode 0


type MaybeTree a
    = MaybeNode a (Maybe (MaybeTree a))


type ResultTree a
    = ResultNode a (Result String (ResultTree a))
