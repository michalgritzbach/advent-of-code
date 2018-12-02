module Year2017.Day02 exposing (..)

import Advent exposing (Test)
import Utilities exposing (pairwiseCombinations)


-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List (List Int)


type alias Input2 =
    List (List Int)


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    let
        isNotEmpty : List a -> Bool
        isNotEmpty list =
            not (List.isEmpty list)
    in
    string
        |> String.lines
        |> List.map (String.split " ")
        |> List.map (List.filterMap String.toInt)
        |> List.filter isNotEmpty


parse2 : String -> Input2
parse2 =
    parse1



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    input
        |> List.map
            (\x ->
                let
                    max =
                        Maybe.withDefault 0 (List.maximum x)

                    min =
                        Maybe.withDefault 0 (List.minimum x)
                in
                max - min
            )
        |> List.sum


compute2 : Input2 -> Output2
compute2 input =
    input
        |> List.map processRow
        |> List.sum


processRow : List Int -> Int
processRow row =
    let
        sortTuple : ( Int, Int ) -> ( Int, Int )
        sortTuple ( a, b ) =
            List.sort [ a, b ]
                |> List.reverse
                |> toTuple

        toTuple : List Int -> ( Int, Int )
        toTuple list =
            case list of
                [ x, y ] ->
                    ( x, y )

                _ ->
                    ( 0, 0 )
    in
    row
        |> pairwiseCombinations
        |> List.map sortTuple
        |> List.filter (\( x, y ) -> modBy y x == 0)
        |> List.map (\( x, y ) -> x // y)
        |> List.sum



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example"
        "5 1 9 5\n7 5 3\n2 4 6 8"
        [ [ 5, 1, 9, 5 ], [ 7, 5, 3 ], [ 2, 4, 6, 8 ] ]
        18
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "example"
        "5 9 2 8\n9 4 7 3\n3 8 6 5"
        [ [ 5, 9, 2, 8 ], [ 9, 4, 7, 3 ], [ 3, 8, 6, 5 ] ]
        9
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
626 2424 2593 139 2136 163 1689 367 2235 125 2365 924 135 2583 1425 2502
183 149 3794 5221 5520 162 5430 4395 2466 1888 3999 3595 195 181 6188 4863
163 195 512 309 102 175 343 134 401 372 368 321 350 354 183 490
2441 228 250 2710 200 1166 231 2772 1473 2898 2528 2719 1736 249 1796 903
3999 820 3277 3322 2997 1219 1014 170 179 2413 183 3759 3585 2136 3700 188
132 108 262 203 228 104 205 126 69 208 235 311 313 258 110 117
963 1112 1106 50 186 45 154 60 1288 1150 986 232 872 433 48 319
111 1459 98 1624 2234 2528 93 1182 97 583 2813 3139 1792 1512 1326 3227
371 374 459 83 407 460 59 40 42 90 74 163 494 250 488 444
1405 2497 2079 2350 747 1792 2412 2615 89 2332 1363 102 81 2346 122 1356
1496 2782 2257 2258 961 214 219 2998 400 230 2676 3003 2955 254 2250 2707
694 669 951 455 2752 216 1576 3336 251 236 222 2967 3131 3456 1586 1509
170 2453 1707 2017 2230 157 2798 225 1891 945 943 2746 186 206 2678 2156
3632 3786 125 2650 1765 1129 3675 3445 1812 3206 99 105 1922 112 1136 3242
6070 6670 1885 1994 178 230 5857 241 253 5972 7219 252 806 6116 4425 3944
2257 155 734 228 204 2180 175 2277 180 2275 2239 2331 2278 1763 112 2054
"""
        |> Advent.removeNewlinesAtEnds


main : Program () ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input_
        , parse1 = parse1
        , parse2 = parse2
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }
