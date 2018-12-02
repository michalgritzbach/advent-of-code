module Utilities exposing (pairwiseCombinations, unique)

import Set exposing (Set)


combinations : Int -> List a -> List (List a)
combinations k items =
    if k <= 0 then
        [ [] ]
    else
        case items of
            [] ->
                []

            hd :: tl ->
                let
                    appendedToAll item list =
                        List.map ((::) item) list
                in
                appendedToAll hd (combinations (k - 1) tl) ++ combinations k tl


pairwiseCombinations : List a -> List ( a, a )
pairwiseCombinations =
    let
        toTuple list =
            case list of
                [ s1, s2 ] ->
                    Just ( s1, s2 )

                _ ->
                    Nothing
    in
    combinations 2 >> List.filterMap toTuple


unique : List comparable -> List comparable
unique list =
    let
        uniqueHelp f existing remaining accumulator =
            case remaining of
                [] ->
                    List.reverse accumulator

                first :: rest ->
                    let
                        computedFirst =
                            f first
                    in
                    if Set.member computedFirst existing then
                        uniqueHelp f existing rest accumulator
                    else
                        uniqueHelp f (Set.insert computedFirst existing) rest (first :: accumulator)
    in
    uniqueHelp identity Set.empty list []
