module Year2017.Day09 exposing (..)

import Advent exposing (Test)


main : Program Never ( Output, Output ) Never
main =
    Advent.program
        { input = input
        , parse1 = parse
        , parse2 = parse
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


type alias Input =
    String


type alias Output =
    Int


type State
    = Start
    | Group Int Int
    | Garbage Int Int
    | IgnoreNext Int Int


parse : String -> Input
parse input =
    input


compute1 : Input -> Output
compute1 input =
    input
        |> String.toList
        |> List.foldl processChar Start
        |> getSum


getSum : State -> Int
getSum state =
    case state of
        Start ->
            -1

        Group _ sum ->
            sum

        Garbage _ sum ->
            sum

        IgnoreNext _ _ ->
            -1


processChar : Char -> State -> State
processChar char state =
    case state of
        Start ->
            case char of
                '{' ->
                    Group 1 0

                '<' ->
                    Garbage 0 0

                _ ->
                    Debug.crash ("Start, char " ++ toString char)

        Group n sum ->
            case char of
                '{' ->
                    Group (n + 1) sum

                '}' ->
                    Group (n - 1) (sum + n)

                ',' ->
                    Group n sum

                '<' ->
                    Garbage n sum

                _ ->
                    Debug.crash ("Group " ++ toString n ++ " " ++ toString sum ++ ", char " ++ toString char)

        Garbage n sum ->
            case char of
                '>' ->
                    Group n sum

                '!' ->
                    IgnoreNext n sum

                _ ->
                    Garbage n sum

        IgnoreNext n sum ->
            Garbage n sum


compute2 : Input -> Output
compute2 input =
    input
        |> String.toList
        |> List.foldl processChar2 Start
        |> getSum


processChar2 : Char -> State -> State
processChar2 char state =
    case state of
        Start ->
            case char of
                '{' ->
                    Group 1 0

                '<' ->
                    Garbage 0 0

                _ ->
                    Debug.crash ("Start, char " ++ toString char)

        Group n deleted ->
            case char of
                '{' ->
                    Group (n + 1) deleted

                '}' ->
                    Group (n - 1) deleted

                ',' ->
                    Group n deleted

                '<' ->
                    Garbage n deleted

                _ ->
                    Debug.crash ("Group " ++ toString n ++ " " ++ toString deleted ++ ", char " ++ toString char)

        Garbage n deleted ->
            case char of
                '>' ->
                    Group n deleted

                '!' ->
                    IgnoreNext n deleted

                _ ->
                    Garbage n (deleted + 1)

        IgnoreNext n deleted ->
            Garbage n deleted


tests1 : List (Test Input Output)
tests1 =
    []


tests2 : List (Test Input Output)
tests2 =
    []


input : String
input =
    """{{{{{{{{<a}!!!aa!!!>ua,a,!>{!>!!!>a>,<!>,!!!!,!!!"!,!a!e}!>!>,<!!oi!!}>},<!!'u"!!!!!>,<e>}},{{<}eaa'<!>},<<>},<}<!},!>},<e>}}},{{{{}}},{{{<,!>,<!!}!>!!!>!!!>!>!>a!!,!!',>},<e,!!!!!><'!<,>},{{{<!<!>i'u">}},<!e!!!>!!eo!!""'!>,<a!i!!!>!!,,>}}},{{{{{{<o!!''!!!>!>,<e}a!>},<>}}},{{<aeo!!!>!!!!<!!!!!!!>,<uu!>},<{!!!>>},{<>}}},{{{<},!!'>}},{{<!"}}!!!ooe'!o!!!!<>},{<u!'aoa!!u{!o!>!>},<!!!>'>}},{{{<!>,<!!!>}!!!>u>},<'a,!>,<!{<!!!>e!>!u>},{{{{},<!>e!""!!'e">},<{e!!!>!}'}o!>,<,o!>},<!!}!>,<i>},<!>},<!oo'!>},<}}<'o}!>,<'!!!>'>},{<{e>,{}}}},{{},{}}},{{<uu!!!!o!>,<!<!!o!!!>!>e"'!>},<!!e!,!!!u!!!>>},<,!>}}<<!!!>,<ai!>{!e>}},{{{<a'!>},<o{ie!>},<!!!>{}au!>},<!!!!iu!ai>,{<!!{',>}},{<eu!o,!<}!>i!"!!!>a,uu!ao>},{<!!!>,<oe!>{e!!!>},<!!!!!!!>!!!>!ei!!io}!>},<"!!o!}>,<!!!><!!!><!!!!!!!>"!!!o!>,<!"!!!>,<!>},<>}},{},{{{{{<"!!!!!>>}}}},{<{!>},<}i'!!i!>!'}<!'!{}>,{{<!!!>ioi"u>}}}}}},{{{<{"'e<e!>},<!o!!,!,'!!!!!>>}},{{{{<ia'oa!!i<o,!>u!!!>ao'a>},{}},<!>},<!!!>!>},<!!!>"!>!>},<u,!>,<eauu}!!}o">},{{{<}',"'>},{{}}},<{<>},{<{!!"}!!!>!!!>{!,"!>},<!>,<!}!!!>ae}'>,<!!<u{!!!>!!!>},<o!!!!<!>,<!!}>}}},{{{{<o!!}!>},<a!>i!!!>},<e!e<,>}},{{{<"iao,}>},{{{}},{{{}},{<!>},<!!i'!>},<o!!{e'!{e"!!u"!!!>>}},{{<>,{<!>o"!>!>},<,ea>}},{{<e'>},{}},{<!<!!!>!>},<!>},<{!e,!>},<},e>,<!!!>"!!u{u{!>,<!uuuoi!!!>!>,<>}}}},{{{{<!!!>},<<"!!!>},<iu!>}e<io>},<e!>,<!!}}>},<!>a{{!!!>,<{!a}ua<',!!!o!!!>e,!!!>>},<!!{a!>},<e'oe!!{u!!u>}}},{{{<ai!!{}ai'>,<}!>{!>o!>},<!!!!!!!e!>'}<!a>},{<!!o}!!!>"!!ou!!o>,{<!>},<!>},<>}}},{{<io>}}},{{{<"!eui!>},<!!!>!>o!!!}>,{<o!!<ie!>!!!!!>!i!'!!e>}},{{<!!!!!>i!>,<,""}a}{!!!>{u!>,<!!'>},<!!!>,a!!!>,<!!iu<}!!!>>},{{{<!!!!'!<ao!>,<}>,<e!>!!!!!>i>},{<,a!{{{!>,<!>!!a!>},<i,!!'>},{{},{}}},{{<!!!!!>u!>'!>!!!>,o!>,<!>a!!,<!>},<!!',>}},{{<!'u}{>},<'!{'u!>,<!>},<!>,<!!!>},<>}}},{{{<!!e!>'>},<!!!><!"e!!!!ea<,ue>},{{<i!><u>}},{{<!>a>,<<"e,u!!!'i!'!!!!!!}!>}i"aa>}}},{{{<u!>ai!!!>!!!>'!>},<e!>!>u"i>,<aeiu}>}},{{{<,i!!!>!!e"au!!'"u>},{}},{}}}}},{{{{},{{<,!>},<<!!!>"!>,<!>,<<!>!!!!"!>,<!>,<>},<"<<{},!!!o,o!!!>!!!>!>},<>}},{{<{,<!!e!>{>},<"oi,!!e!!!>i!>,<{!>,<!>},<!!!!o>}},{{{<,{!>'i!>!!!>!!!!!>},<'i{u>},<!>},<!>uo!!o!>,<o!!e!ae!!',!>,<>},{{{<u!!!!!>"u!!{>},{<!<{uo!>},<<!>},<>}},{}}},{{},{{{{{<,,>},{}},{{<i'!!!>!>!!e!>o!!!!!!!e'!!!><!"",!!!>"u>}},{{<iiu!!{!!!>'aa!!!>},<!!!>!'}!!!>>}}}},{{{<i!!ae<!!u<a!!"''!>,<>},{<!!!!!>!!o!>a,<!!{!>!>,<,!!!!>}}},{{{{{}},{{<!!'eo!>!o<,e!>,<!>},<!!>},{{<<u',}!!!>!<!i!>},<!>>},{<e<e!>},<}!>,<ii!!<"!>},<!!!>>}}},{{<{}!>!>i!>,<!>!!!>!!!!"{!>,<,'!>},<>,{<!iee!!!!"ao{",!>u"!{!!!!}>}},{<{>,{<!>},<!e">}},{<a!!!>},<u>}}},{{{{<!>},<>},{{<!<e!!!!!!u!!!!!ea!!ue!!u}!>},<!>,<!>},<!!!>a>}}},{{<a!>,<ua<!>},<'e>},{<a!!!!!!!>>}}},{{<i!!!!<!>,<'{,!{!!!!!!}}e!!{<io>},{{<,o"e!>},<!>},<i{i!>},<!!>}}},{{{<a!>}!!'a"<ou>}}}}},{{},{<,!>},<>}},{{{},{<>}},{<!a!!!>u!!a>}},{{},{<!!,'>,<}o!!!>!!!>>},{{{<<,,!!!>!!!>!>},<e!!{<!>i!!!>o!!!>'!>},<!!!>>}}}}}},{{{<!"!"!,!>},<!!!>a{e!!!>!!!!!>>,<!>,<!>},<!!"!>,<ii'!!{'>},{<!>},<,>}},{<o!!,!i"'!>,<>,<ie,!<"oeu,'!!!>!>},<'i'>},{{<!!,!!{!!eai!!!aa!!!>'<!>},<!!>},<!>},<o!!!>,<iu}">}}}},{{{{<e>},{{{}},<!i!>,<i'!i!!!>!!!!}!!!>'<!>},<uo!>>}},{<,!!!!!a!>},<!uo!!!>,<!!e!!!>>,<>},{<>}},{{<e!!u!a<{u>,<<!!!>!>},<{!!!>,{!!!!o!u",!!!>'o!!!!a!'!!,>},{<}!>,<u!!,{!e'!!!>,<e!>!!!>u!!!!!>!>},<!!!>a!!!>!>>}},{{{},{<uueua!!!>!>},<!!!!!><!>,<!!!>e}<>,<}{uuo"!!o!e>}},{{{<}!>"!!!>{o!!!!<>},{},{{}}},{},{{<{!!!>eo!>},<,>},<!!{<!>,<{iu{!,!!oi!!"!{'!!>}}}}},{{{{<!!u!!!!!>e,"!!e!>!!,'!>,<>},{{{<<!>},<!!!>u>},{<!!!!!>"<u!'!!!<!>,<a{!!!>,<>}}}},{{<!>,<!>i!!a!>},<e"!>>},{{<<''o!>!!!>>}}},{{<!!!!u!!!!a,!!<"!>{ao>},{<<o!!!'!>,<u!>,<o!u!>{!!>,{<},,o}!!"ioo!>,<}!!,o}i!>,<<a">,{<!!!>!!!!!!!>!>},<,{!!ia!>,<!!!>!!!>a!!{}o>}}},{<a>,<!>!!o>}},{{{{{<{o!!e'}{,<!!u>}},{{<o"<!!!>!>},<i{!!a!!!>}'o{!'{>},<!!{u!a!>,<"!!{>}},{{<!!!>!!o!!<!>!>},<"!>a{!>},<'!>},<!>},<!!!uo">,{<}!>},<!!i!>,<!!!>},<!!!>!>},<!!!a!<!>i!!!>>}},{<}ia"!!!>o,<",!,>}}},{{<!>,<<a!>,<>,<}a<!!!!}!>},<u'"!!!>},<'!e>},{<!ue!!!>},<oe>},{{}}},{{}},{{{{}},{{<<<,!!o!!!!e!!!>}!!!>!>,<ia'"'"!>},<e{>}},{{<,oe!a}!>>},{<{!>},<!!e!!!>!>!!o!!!>u'"!>,<!>>}}},{{{<<'!>},<e!!!>!!!!!u!{e!!i!>,<!>},<'!!}!>,<!>},<!>,<!!!!!>e>},{<"!!!>,<!!!>},<!>}'!!"!>},<!"a<!!!>,e!}oe>}},{<">}}}}},{{{<!!}'!!{a!>,<!>a>},{<!!i!}!!!!!>!>,<e!!!,}!>{!>,<u!a!>,<>}},{{{{<!>!>,<>},{{<">}}},{{{<,i!!!>}!>,<!>!>,<!!!>},<e!>},<!>''!>>},{}},{{<!>,<{!!!>'!ai!!!>,<>}}},{{{{{<!'!!ei,!>!!!>!>,<!>{!}'o!>!!ui}>}},{{<!!!>!!!>,<e!ou{!>},<ei!!!>!uu!a!>},<>}}},{<<o{{"!!!>o!>,<!!}!!<!>,<}o'!>>,<}!,,},!>},<o!!{<"">}},{<!!!>{<!!!>,<'!>,<i!!!>o!'o>,{}},{<ui!ou!{o>}},{{<,!>},<!!!><"!!!!!>''>},{{<ou}!>,<!!}'!!"<!!u!!!}!!e!!"!>,<!{!>!!!!!>},<!!!!!>,<!!!!!>>}},{{<e!'>},<o{a!!!>,!!'!>},<!>},<!>,<ue""!><<<i>}}},{{{<<!>},<<!!!>,<"oai,<o!!!>!>>},{<'ue!!!><!!!>{!>},<e!!,!!">}}},{},{{{},{}},{<!u!>,<!"!>},<{o!!">,<>}}},{{{}},{{},{<,u!!'!>!>!>},<}!!e!{!!u!!!>},<!>!>},<,>}},{<>}},{{{<u}!"!!'u!!i!>,<!!!>}!!!>!''!>!!>},<!!u!!{,ui!{!!!!!>!!!>},<!!!!>},{<iie,!!!!!!!>'>,<!!a!!e',!>u{'!!,<!!!>'>},{}}},{{{{{{},{}}}},{}}},{{{{{<!!u!!}!u!!!>!>},<!>!!u!!{{e<'!>},<'!>!>},<>},{<"o!>,<!>,<!,!>,<u>}}}}},{{{{<e{<e{!!!>!>,<ii},}!>,<a>}},{<'au!>},<{{aaiu!!>},{<!i,ui!!!!!>i"!!iiiu}>}},{},{{<}!!'<ua!!!!!!o}a!>,<!>},<>,{}}},{{{{<i!>iie!o"!!,!>,<!>,<!>,<>},{<i{!>e!>'>}},{}},{{<'!>},<'!!}!!a!>},<e!!a}!{{'!>e>}},{{<}i"'!>,<!!!>!>},<!ae{!!o!!!>!!,!!!!!>,<!!>},{{<i<!!a,!'>,{<"i!>},<!,!>'euu!!!>,<,>}}}}}}},{{{{{{<'u!}!>!>},<a>}}},{{{<!!ee!!"}>,{<eu,!!!!'!!!>!>,<,{oee}<!>},<i,>}}},{{{<u!>,<!!!>ai!>},<""e!!!>!>,<!u>},<!!!!!!'!!'!!i!!"ei,}!!!i<,!>},<!>>},{<a!!!>aui!>},<e!<!>}"}!>,<,!!!>!>},<!!!>!!>},{{{}},{<!>!ue"a,!!!!!>e!!,!>!>,<<>}}},{{{<!>,<"!!{,<!><!>},<<!!iio!>},<>},{}},{{},<a,"!>,<}!>,<a!!!>e!!">},{{<!>!o!!uu!,!>,<u>},{<!a!!!>!>,<iu!o!!ia!!!>!>,<!!!>,<,o!!!>!>!!!>e{>}}},{{{<!>o!!!>}oa"!!!>!a"!{>,<i>},{{<'>},{<ee!!!>!!!a!!ia!,!>,<!!!!}!!ui!>u<u>}},{{}}},{{<a<!e!!e!!!!!>!u!!!>"!>,<o!!}!>>}}}},{{{{}},{{{<!!}{o!u{!<o'}u!>,<!!!!<>}}},{{{<e!!!>!>},<!>!>},<!u>}},<i'ei'!>},<!!oo<i!>},<!!!>,o{!>>}},{<"e!>,ia"!>},<u!!!a"!}<ea!!<!{>}},{{}}},{{{{{<!{aiioi<!ei!>!!!>},<a!!a!>},<!u!!!!!!,!}<a>,{<'oo!!!>!>,<!!!!!>!u!>},<>}},<"},!!!>,!>!!!>!!!i,>},{{<{!!u">,<{,{!>},<{!}'>},{{<<o!{'!!!>,<>},{<i'oo!{u!!">}},{<oo>,<<>}},{{{{<i{!>,<!>},<"<!!!>e<e!>,<!!!""<!!'o>}},{{},<e"a!>},<au,>}},<!!i{!>,<!!!!<>}},{{<"<o}'e!!!!,<>,{<>}},{<>,<!o!>},<"!!!!!>!"{{"''au!!o,<!!!,>},{{<!!!>!>!e<o!>,<>,<>},{<>}}}},{{{{{{<"}>},{{<!!{u!!!>,<!>},<<!!!!"'>},{<!>},<'"}o,o!>},<!!o!>},<!!ie'!>}!>,<a,>}},{<{'e,a!>i!>,<!!!>,<!!!!"'a,o>}},{{<i"i"!>},<!!,!!!>!>'a!{!>,<!!!>,<!>},<e>},{{<!>,<>}}},{{<'i!>,<,o,!!i'i!!e<!}o>},<!>}e{!!o<!!!>o!!,!>},<,!>,<!>},<!>},<o!!{>}}},{{<!i{!!,u}'u!ea>,{{<!!oo>},{<,>}}},{{<!>,<!!i!!!>'!!u}!!,,u}<!!,!ea>},{{<!!i!!,"!>'o"{ou,'}>},<!!!>>}}},{{<e'!!<u>},<!>,<i}!>!>,<<!!!>"{u<uo>}},{{{},{{<u!!euu!>},<>}}},{{},{<!!!>a<<}{""'!!!>>}}},{{<}!{'!!}}e,'e!!'}!>},<!!e>},{{{<u!a,eoo!>,<{!!,<>},{<!>,<>}}},{<!<}uei}o,io!>,<{!!!>,<!>>,<!!{oioi!>,<!!,!>},<iu!>},<!!o,}!u<>}}},{{{{{<!!!!!!e!>},<u!!i!!!>a!">,<a!>},<}>},{{},{<e!!!>>}}},{{},{<i!!>,{}},{<""{!a>}}},{{{<a,"!!!>!!'!au',a!'aie'!>,<{o>,{<<<<!!e<>}},{{<e'"!!a""!!>},{<a!oe!>},<,<>}},{{<}i'<!>!>,<}!!oa!!eeu!>},<a>}}},{{},{{{{}},{<!>!!<!!}>}}}},{<!!!!'i,<!>},<"{!!"},>,{<!e'!>},<,!><!!{"!>!"!>!!'>}}},{{{<!!io!>,<ioi,>},{<i{!!!>,<!>!<!!!!{ae!!!>!>},<e!>!!!>'a,"!>},<>}}},{{{<>}}}},{{{{{{<{!>},<!!!!ua!!{e!>a>}},{<oa!!u{,!>},<e!>,<!>'o'u>,{<!!"!!!>,<}!>!!}!!<"ui!>,<!"!>},<,ou>}},{<o!>>,<!>},<{!!,a!>,<!,aa!>'!!!!!>"!o!>,a>}},{{<!"!!!e!>,!o,'<ea'i!>>,{<!>},<'!,!>},<e}>}}},{{{<!>,<!!}!!!>,<a!!<e}!o!>i,!>},<,>},<o,>}}},{{{},{{<!a!!!>!>},<o!!e<!!oa!!}!!>},<}!>!!!!i{>}},{<<"!>!>,<<<!!!>,!!!>!!!"{a>,<!>,<!!,!!!!!>>}},{{{{<>,{<{o{i'<{!!"i<!>,<u!>>}}},{{<u!>,<!>,<!!,!}!ee,!!!!'<'!<!><>}},{{{{<!>},<'!!!>ue!o!>!!<!>},<<!!>},<!!!>}>},<!>!>u<>},{{},{<!>},<!o!>,<i<e}>}},{{<u"u<,u"'a}<i!!!!!>e}>,{<!>,<!!!>>}},{}}}},{{{<!>,!!!>e>}}},{{<,oe>},{{<<!o!!!>},<!>,<u!!}!!!>'!!'"!!!!!>>}},{{{}}}}},{{{<!>},<!!a!>,<!>!!}a"<'>,{<!''u!!!>},<!!'aa!>i!!o'e!!!a>}},{<!>},<o!>i'!a!!<aa>,<'<e>},{{<'!!!>},<}i{i!!!>!!{!!a,!,>}}},{{}}}}},{{{{<e}!!>,{<e!!!>!!!!!'u{!!"!!!}o!'aou!!!!!>a>,<!!o}}ai!>,<i>}},{<!>},<!>,<'!!!>u!o{oo{'!!!>>,{}},{{<!!{!""o,}!!!!{!o<!,ii!!<,!!u,<>}}},{{{<<!>,<!>},<!>,<!!}!!!a,<,o!>},<!!a}!>,<!{}>}},{<"}{!>,<!a<<o!{u}!!!!!!!!"<!>,<!!<!>>},{{<<!!,"!>!!<!>,<a!>},<e,!>},<<{!!>},{<<!>,<!!{!!!o!}!>,<!!!>},<!o>,<oo>}}}},{{<{o!!!!!>{!>,<}>},{{}}},{{<{!!!>,<<!>!!!!u!>!!!>'i{,}u>}}},{{{{{<!>},<!!!>e!>},<oui">}},{<,,!'!>,<o!!!>ua<!>>}},{{{<{!,!>,<!!!>"!!!!<!>,<<!!<a!!''!>>}},{{}}},{{{<!!u}{!>"!!!>},<>},{<!!!>u'ea!>eu!u!!!><u"e,>}}}},{{{{{},<!!!>!u!!!>},<,{oi!>,<'!!!>,<>}},{{{{<!>,<"u}!!">,{}}},{<i!i,{{!>,<{!!!>!>!!uuo!>,<'!>,<>,<!!!!!><ioe!<!!ae'!!!>},<,>},{}},{{<<}!!{!{!!>},{<!!!>!ioaau!>!>},<ou,oo>,{<!>{e<,iei!!au!!!><u{!>},<>,<!!!!!>'!""!!!>e{!!!>u>}}}},{<!!o!!!,<!'!!!i>}}}},{{{{<a!!!!!!,iai!!!{!!o!!e<uo!!!><>},<}!!'!>,<<a,!!},!>,<!!!!!>}'!>,<!>u>},{<!!!>!<o!!!>{o!uaa,!!!>{!!!!}<o!!!>!>},<>,{<,"{ae!>},<!>},<!!!!!>!!!>!!!>}e,>}}},{{{<<"!>'a'!!uu>},<,!>!>a!>!!>},{<!!!!'!!!>e!!!!!}uu<!!"!o!{ie!>"eo>,{}},{{},{{{},{}},{<!>,<'!>e!}!!a<o>}}}},{{<!!!!!>},u!!!>e<{!!u">},{}},{{<,'i!!!>>,<"e<,'e{u!!!!o}"!""uu!!!!!>!>},<'o>},{{{}},<o!>},<!!!>!!!>e,!!!>,<!>},<,!!!>,<a>}}}},{{{},{{}},{{{<>}},{<>}}},{{},{{{<ueao>,<ou!!!>},<!>!>ei"e<i!!>},{<!!!>,<!>,<u<,!!a}<!>,<e>}}}},{{{{{{{<"!>">}}},{{<{!,}!>!!!>,<"!>},<!!!i!>},<i!!'!!uau!!!>!>},<>,{{}}}},{{{<>,<!>,<!>,<!!!!ioi!!!><{{}!!!!!!'>}},{<!!'!!{o!!i!!i}o!!!>},,>,{}},{{{<>,{{{<e!}{!!o""!>u!!!>">},<!!'{!!!>"!!!!!u!!!<,!!!>>}}},{<!!!>!!a!>},<}u!!!><!>,<o!!!>!uo}"u!{!>!!<>}},{{<a!>,<ui!!<!!!!{o!>}}!>},<"e,>,{<a<aeu"a!!"u!!i!!u!>},<!>,<!>,<'"<a<>,<>}},{<!>},<}"e'a!>},<!!'!>,<e!a!!!!o'!{!>},<!>},<'>}},{{{{},<<<o!>},<!>,<!!}!!!}>},{{},{<u!!'}i!!!!>}},{{}}}}}},{{{<"!!e!>},<i}!>i>,<!>},<!>!>},<>},{<!!oeou!>},<i!au{ai!!!!!>!>!>,<!!!>>,{{{}}}}},{{{{{<e!,,,!!u}i!>,<e,}}o!'>}},{<!!!>>}},{<{},!!iue''o!!!!!>!>},<>}},{{}},{<a!>e'!!,!!!>},<>}}}},{{<>}},{{{<'}{ae>},{<!e!>,<<}!!!>a!>},<!>,<!!!>!!!>a!!o>,{<!>},<<{a,e,,i}o!>,<>}}},{{}},{<ui!!!!ui!>,<{!>},<>,{}}}},{{{{{},<!!!!!>,<a''ao!>},<{!",>},{<!>!!u!!!!!>a!!!!!>,>,{<!>!>,<!!!!!>i}u!!!>!!!>,<!!uu!>},<!!>}}},{{{{<iai,!>'!>,<,u!!!>},<!!!>!!ao>},{<!!!>{!!!u!!',a!!!!!>u!>,<{>}},{{{}}}}},{{}}},{{{<i!>,<"!>,<{>},{{<e!!a!!!!!!}!!!>!!i<!!!>{>},{<!!!>!!!>}!!a!{,e!>a!!,!!!>>}},{}}},{{{{{<e!!iue<e!!,>}}},{{<o!>},<,!>,<>,{<i,,"{a!'}{o>}},<!!!>,!{!!!>},<!!oa!!,oa{!>>}},{{},{{<!!!!e'!>},<{!>,<!!i!}{a!!!><uu,!!!>},<}!!">,{}},<!!eo!>,<!>,<,o!>,<!>},<,!!!>}"o<!>,<i!!!!!!!>},<>}},{{{},<oae!!!e!>,<>}}}}}}}},{{{{}}},{{},{<!{'ie'!!!>,<!>,<ou!>,<!!!>},<{ea!u!>a!!!><>,{{<o!!!!!<,i<!>},<i!}!}!!!>,<!>u{!!!>>}}},{<!!!>a!>,<<{e{!>},<}}!>>}},{{<!>,<e>,{<!>},<!a!>a!!!>!!!>o!uio{o>}},{{<!!!>!!o!!iu!<"}!!o{'!{ooio<>},<e>}}},{}},{{{{<>},{{{},{<!!!>},<i!!!!!>!iaa,}e!>!!o}a!!!>},<>,<e!>},<!>,<!!i,,,"'uo>}}},{{<>,<>}}},{{{<!ee!!!!!>!>},<'!!!>!>i>}}}},{{}},{{{<}!!!>{!!>},{<'a,!{i!!,!>!!'!>}u>}},{<!!"!>!>,<">,{<<<!><o!>},<!>},<e!!<"!>>}},{}},{{{<!!!!!>!!!!!!<"<!"{}!>},<!!!!!!!>e}'!!{!!i>}},{{<!!i!>>,<o"!{'!>!>'!>,<!!<i!>},<o!>},<!!!>,<!!}a!>,<!!>},{{},{<!!a">}}},{<!>,<ii!!,>,{<a<o"!>>}}}},{{{<!>,<o!!!>}>},{{{<u>}},{<!>},<!>},<>}},{{<!!!!!>ao!>},<<!>},<!>,<!>,<!',o!>,<!!{!>,<!>},<!>},<{>}}},{{{<}!>},<<a>},<,!!!>ua!>!>,<o{u!!!i!>>},{{<'o>}},{{<e!>},<!!!>>},<!>!>,<"<!!!!i}>}},{{{{},{{<!!<o<!>},<>,{<>}},{{}},{}},{{{<>},<eoeuo!>,<>},{{{<o!!,!>,<!>"o!>,<,!!!>o'!!<oe>},{{{},{}},{}},{{<!>!>},<eo>},<>}},{{<!>!>",!!!>,<!!<,!>>},<!!!>{!>},<}<uo!!oe!o<o!>,<"{"<>},{}},{{},<!!{!!!!!>!>i!>'{ei!!!a!!!><!!{!!!!!>}<>}},{{{{}},{<i!!!>!>},<!>a!>},<"}!!,u,'{a!>},<{>,<!!!>io!!<!!{o!!!!!,e,>},{{<e>},{}}}}},{{{{{<u!>,<">,{<<!>uo!>,<>}}}},{<<u}"!<{{!!}!!a!!">},{{<}!!!>},<i!>},<!>},<}>},{<!!ui>,{}}}},{{<}"o!<ae!!'!>},<!>},<'u'!>},<<>},{{{<ai,}io!!!>e!>,<a,>}},{{<!>!><a!!!>o!!!>},<,{,a<u!!}e>}},{{{<'!>}!!'u,!!!i!!u>}},{<!>,<o!>!,>}}},{{{{}}},{{<!!!>,a!!<iae!!!>{<!!ao!!!u>},<!!!>!!<!>i"!>,<!!!>!!iu!>},<!!"'"!}!>,<<,>}}},{{{{{<iu!>},<'aae!o!!!>{aa,!>!!!>,<>},{<u!!!>,io<{u!!!!u>,{}}},{<}!<!!}!!!>!>},<}!>!>!>!!i,!>'!>,<,">,<{ioo!>},<!!!>{}a'e"!o>},{}},{}},{{{{}},{{<!!ue<>},{{}}},{{<a!,>,{}}}},{{{}}},{},{{{<o!!!>!>,<!>i!>},<,e!!!u<!>},<>,{<{<!!!>a!!!!'oi<!!!>!!}!>},<!!!>{>}}},{{{{<!!'u,>,<!!"!!}"!!{!i>}},{{<!>},<{!!<}<e>,<u!>,<!!!>!!!>!>,<a<!!!a!!u!a"!!,!!''>},{{{{},{{<!>!!!>!>,<e!!}!><!>,<!!!>,<'!>e!>!!ee,>}},{{{<,o!,,o<!!i!u!>>,{<!!!!ii"{}!!!>',>}},<!><"a!!!!{a!>},<e{!!!>}}>},{{{<"',!a>},<!>},<'!>,<!>,<{i!>},<!}!!!!!!!>!>,<i!>!'!!}<>},<{!o"o>},{{{{<i!!!!'{>}},{{<!{!}!>e!!!>!!!>i'e,,a}!>u">},{<!>},<u!>,i!i!!!>!>,<,a!!'"",uu>}}},{{},{{<!u!>a"}>},{{<!<iu!!!!!!u'''>}}}}}}},{},{<'!oue!!!>oa!u!>,<}e!>{o>}},{{},{{{<!!!!!eii!!!>},<>}},{{<}!>},<e<!>!!!>i,!!!>},<!e!!!!!>e!!!!!>">}}}},{}}},{{{<!!!>"!,!>,<!>!!!>,!!!!!>!!!!!!!!'oa!!u{a"!!!>e>},{<{,!!!>!>,<<!!!>,<"!!!>!>},<!!a"o}'>}},{{<>},{{<!>},<"!!!!!>'!>,<!>>},{<!!!>!>,<e{,a!!e}!>,<u<}!!i!>},<!!!>'>}}}}},{{{{<!!!>o<!!!>!!!!e"!!",!!!!!!i!!!!!>!'>},{<!!e!>,<'!!!>>}},{}},{{{<!'!!!>!ao!>,<!!ia!>},<!>i!>},<'!>,<>}},{{{<o!!,>},{{}}}},{{{<"!>,<!<<!>,<'>}}},{{},{<"!a!>},<ia{!>!!!>{!!!>},<!!}a"!!'!!'"u>},{{<a"!!!>},<'!!!>i!>,<',}}>},{}}}},{{},{{<o!!!>!!!>!i}''eu!>},<!!a,!!!>>},{<{!!!>o'u'oa!!e,>}}},{{<!>,<!}<}"!!!>u,!>!!!!!!!>!!!!o>,{<{!>},<!>'e!!"!!e>}},{{},{}}}},{{{<!>,<!!!>",!>oo{!>},<e'!!!!!>>},{<!>'a!>,<eu!!!>u}!!!!u">}}}},{{<!o'!i!>,<,!!",a!>},<>,{<{}oe!!!>}{!>},<,{'!>},<u!!!!!>,<>}},{{<'ao'a!!,!!<i!!!>>},{<i}!>,<'e>}}},{{{<<!!!>,<i!!!!!!!>}>}}}}}},{{{{{{{<{!>,<o"u}!!!>{o>},{<ue!!{>}},{<!!i!>},<o,!}eo'!!ue!>},<e>}},{{<{{!!i{!>},<!!!>!!!!!>},<e'<'!!{!>},<>,{<a!>,<'!>{!!!>"!}"<a}!!!>!!e!!ii!>>}}},{{{{{}},{<!!!'!!!!!>!,!>,<i,,i"{,e!>,<!>,<,>}}},{{<<ai!!!!!!!>!!!>},<"'o!>,<o'!!!>,<!!{}>,{}},{{<!!!>!>},<i!>},<i>},{}}},{{<<{>},{<!!!>,!!i!!ea<}!!!!o{}a!>,<,}!>>}}}},{{<"a!>},<ee,!>,<}!!!!!>'eoi{!!}!>},<>},{<!!i}!>!>},<!u!>!>,<}",!!'!><!>!<>},{}},{{{<u!>,<ae!i!>,<!!e!!!>iia>},{<!>,<!!!>a{u}ieu!!a}!!!!e!>},<o!>},<!>u!!u!>},<>}},{{<!!!>>,{<'>}}},{<!!u!>},<,!!o!!!>!!a!!!>!!}>}},{{},{{}}}},{}},{{{<e'o!>},<!!iie!>,<,!>,<!>!!e!>},<!!!!!!!>,<>,{<!!!!!>u!>,<e!o!e!>!!!>!e>}}},{<<!>,<i"!!}!!o!!!>!e{aaoui>,{<,}i'i"}"ou>}}},{{{<>},{<!!i,oa!!!>!!!!!>}}}eu>}},{<iaa!!!!i>},{{{},<<u!!!>!!o!>,<',!!!>u!!!>!!'{!>'!!,e>},{}}}}},{{{<}!>,<!>!!!>},<>},{{{}},{{<{,!>u"!!a"',<}!!!>u!!!!!>a!><!e>}},{<!>,<!!"'}!oa}uu}!{e'ei!!oa{!!!!!>>}}},{}},{{{},{{{<!!e">}},{{{<!!!>!!,">}},{},{{<a"oe!!!>"<!>,<!>,<">},{<a!>,<i!>!'!!!>u'!>,<,!>,<o,>}}}},{<"<!!!>iii!>!!!>},<<!>,<!!!>},<!!o!>,<!!!>ui">,<<i<a!!'"}<e!!!><!!!!!"}e,,!a>}},{<euo<!!!>},<u>,{<!!,!!u{!!!>!!!!!!!!}e<>}},{{<'}i!>},<a"e!!!>ue!!'oa>},<!e!!!>>}}}},{{{<'{!>,<a!ai'!!!!!><u!>,<"!>},<>},<a}!>},<}eo!!!>ai!>},<e',!>,<a!>!a"}>},{{<}o!>ao"!}<<>},{{<>}}}}},{{{{{<!!!>!>},<!ai!!>},{<{>}},{}},{{{},<i},ue!!!!ou!ee!'e'!!'>}}},{{<!>},<ii!!!>!>,<!>},<<}!>},<!,!>},<"}a>},{<,!!!>}!>},<!!,!>},<!>u<i!!"!>,<'u!>,<!>,<!!!>>}},{{{{{<"e!!!!!!u!!!>!!"!!!>"!}!oo!!!>,<>},{<!>},<!<!'u!>,<ae>}},{{},{<i!{>}}},{{{<{'!!<}!>,<,i,!>},<!>,<o!!!>,<}!!!!o>},<!!!>!e}a!e!>}!!o>},{<!o!!ae,a!>},<}}!>},<}{!!>},{<ea!>io"'{!<!>,<>,<o,"a'!>,<i!!!>}}!!!>,<!!<!>,<!}{a'>}},{{{{},{<{!!!!!>!!u'!!!!!!!<}e"a}>}},{<!>},<!!!>io!!!!!>},<,e,}!>!>!!e}!>},<ui<>,{{<i}o!!!>!!!!,{!!ee!>,<,!!>}}},{<!>},<,!>},<>,{{<{!>,<{"a}!!!>!!!>!!a!!ai!u>,{<ie"!"i>}}}}},{{{{<!"!!!>!}!!a!!>,<!>u!!!>!!,o!!,<!!<!>,<"!>,<e!!!>,{}!>},<!">}},{{}}},{{{<!>},<a"!!oo!!!!i>,{<uoo!>},<!!!>!!e,e!!u!u,i!!i,>,{}}},{{{<!>,<'i!>},<{e!{!!i>}},{<uiua{!!<e>}},{}},{<io!>e>,{{<i,!}!e>},<,!>{!!!>!a!}>}},{{},{{<a!>,<a!>!>,<oi!!!!!>,<!>!"!!a!!!>!<!<>}}}},{{<"ioe!>,<!!a<!o"!>!!i{!!!>,<!>!!o>}},{{{},{}},{<>,{<'!!'!>!i<!>>}},{{<!>,<!!!>,a<o<{!,}!a!!!>,!!!>>}}}}}},{{<"'",<'>},{{<u},!!!>",!>},<e{e!!!>!!!>!>,<!!'o}i>,<!>,<{{}i"a{!!!!!!!>,<}o,>},<!ii>},{{<{!>,<!!o{>}}},{},{{<'uo!>ui{!!!>!>,<!'!!'!"eo!<!!{!i>,{<!!,{{i!!!>,<!o>}},{{},{<o>}}}},{{{{{{},{}},{}},{{<e!>e!!o!!!>ue"{!!!>!!!>!>>}}},{{<>,{<i",a!"">}},{{<!i!>,<!!!"e!!u!!!!!!!>o}e"}>},{}}}},{{{<},!>,<!!!>uu!!'}!!!>o!!}<{!!!>,<u!}>,{{{},{{},{<<a!<!>},<u!!i{<!>},<>}}}}},{{<!>!>o!!!>!>,<<,!"!!}">}},{{<!!!!'o>},{}}},{<ao!!!>!!!>!!!>!>},<!"aao"{!>},<!>>},{{<'oa{!u}!o!!e!>},<!!'>}}}}},{{{{{<!>},<o}o!!!>,<'>}},{}},{}},{{{},<!>,<i!!e!>},<<<!>,<e{aa!iouie>},{<!!!>,>,<{!!!>,<'!!!>},<"!>"}!>},<!!!>,<!>!!!!!e!!!!,!>,<>},{{<!!!>,<>}}},{{<!>!>},<<<!!e!!!!!>>},{{<}!>},<'>}}}}}}"""
