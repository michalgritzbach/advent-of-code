module Year2018.Day02 exposing (..)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)


-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List String


type alias Input2 =
    List String


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    let
        frequencies : String -> Dict Char Int
        frequencies =
            String.toList
                >> List.foldl
                    (\item table ->
                        if Dict.member item table then
                            Dict.update item (Maybe.map ((+) 1)) table
                        else
                            Dict.insert item 1 table
                    )
                    Dict.empty

        repeated : Int -> List String -> List String
        repeated n =
            List.filter (frequencies >> Dict.values >> List.member n)
    in
    List.length (repeated 2 input) * List.length (repeated 3 input)


compute2 : Input2 -> Output2
compute2 input =
    -1



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example"
        "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab"
        [ "abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab" ]
        12
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
rmyxgdlihczskunpfwbgqoeybv
rmyxgdlksczskunpfwbjqkeatv
rmybgdxibczskunpfwbjqoeatv
rmyxgdlirczskuopfwbjqzeatv
rmyxedlrhczskunpfwbyqoeatv
rmyxfdlicczskunpfwbxqoeatv
rmyxgvlihkzskunpfwbsqoeatv
rmyxgdaihczvkunpfwblqoeatv
nmyxgolihczskunpfwbjqieatv
rhyxgdcihczskunifwbjqoeatv
rmfxgdlihczskunpfwbvqgeatv
smyxgdlihczskunsiwbjqoeatv
rmyxgdcihcxskunpfwbrqoeatv
rmyxgdlihczckuiqfwbjqoeatv
rmyxxdwihczskunifwbjqoeatv
rkzxgdlihczskunpfwhjqoeatv
rmypgdlihczskunpfwbrqoeafv
rmyxgplihczvkunpkwbjqoeatv
rqyxgdlihdzskjnpfwbjqoeatv
rmyxgdlihczskqnpswbjqoeaov
mcyxgdlihczmkunpfwbjqoeatv
rmyxgdlohczspunpowbjqoeatv
tmyxgdlihczskunpfwbeqoeltv
rmyxgdlibccskunpfwbjqoegtv
rmyxgdlehczsaunpfwboqoeatv
rmaxgdlihczseunpfwbjqojatv
rmyxgdlijczskynpfwbjboeatv
kmlxgdlilczskunpfwbjqoeatv
rmsxgdlshczskenpfwbjqoeatv
rmbxgdlihcmskgnpfwbjqoeatv
rayxgdlihczskunpfwbjqoeaef
umyxgdlisczskunpfdbjqoeatv
rmyxgdlihczskunsfwbjqieatg
rmbxgdlihczhkunpfwbjqoeamv
rmyxgdlihczskeypfwbjqxeatv
rmyxgkrihczskunptwbjqoeatv
rmyxgdlihczskunpawbjqoexiv
rmyxgdlihcrskqnpfwbjqceatv
rmyxgblihczskjnpfwbjqieatv
rmyggdlidczskunofwbjqoeatv
rmyxgdlghczskunphwbjqomatv
rmqxgdbihczskunpfnbjqoeatv
rvyxgdlihczsgunpfwbjqoeanv
royxgdlnhczskqnpfwbjqoeatv
rmyxgdlihczskugpfwbkqreatv
rmyxfdlihczskunppwejqoeatv
rqyxgdlipczskunpfwbjqoeqtv
rmyxgdlicczskunpnwbjqotatv
rmyxodlihczskxnpfwijqoeatv
rmyxrdyihczskunpftbjqoeatv
rmtxgdyihwzskunpfwbjqoeatv
tmyxcdliiczskunpfwbjqoeatv
rmyxgdlihczskmnpfwbjjoeadv
rmyxgdnihczskunpqwbjqojatv
bmyxgdlihczskcnpfwboqoeatv
rmysgdlihcyskudpfwbjqoeatv
rmyxgdtihczsmuupfwbjqoeatv
rmyxgdlihczssunpffbjqolatv
rmyogdlihczsklnpfwbjqoxatv
rmyxgjlihczskunpfwsjqoyatv
rmyxgalshczskunpfwbuqoeatv
rmyfgdlihczskunqfwbiqoeatv
tmyxgdlihczskunotwbjqoeatv
rmyxpdzihczskuopfwbjqoeatv
rmyfgdlihczskunpfrbgqoeatv
rmyxgdlwhczskhnofwbjqoeatv
rmyxgdlihczsmudpfrbjqoeatv
rmyxgdlihczokanpfwbjqooatv
rmyxrdlihczskunppwjjqoeatv
rmyxgdjihczskwnpowbjqoeatv
mmyxgdlihczikunpfwbjqoeamv
rmyxgflihczshunpwwbjqoeatv
rmytghlihczskunpfwbjqoeatk
rmyxgdlipczmbunpfwbjqoeatv
rmyxgdlihczkkonpfwbjqomatv
rmfxgslihczskunpfwujqoeatv
dmyxgdlihczykunqfwbjqoeatv
rmyxgalihcbskunpgwbjqoeatv
rmyxgdlinczqkunpfwbjqopatv
rmyxgdlihwzslunplwbjqoeatv
rmypgdlihczskdtpfwbjqoeatv
rmsxgdxieczskunpfwbjqoeatv
rmyxgdlihczskwnpfxrjqoeatv
rmyxgdlihzzskunpflbjpoeatv
rslxgdlihczsnunpfwbjqoeatv
rmyxgdlmcczskunpfwbjqoealv
fmkxgdbihczskunpfwbjqoeatv
rmyxgdiigczxkunpfwbjqoeatv
rjyxgnlqhczskunpfwbjqoeatv
ymyxgolihczskunpfmbjqoeatv
hmyxgdlihczskuncfwbjqoejtv
rmyxgqlihczzkunpfwbjqojatv
rmgfgdlihczskunpfwbjgoeatv
rmyxgdlfhczskunpfwbjqweaxv
rmoxtdlihczskunpfwdjqoeatv
ruyxgdlihczskunpfmbjnoeatv
rmnxgflehczskunpfwbjqoeatv
rmyugdlihczskunpfwfjroeatv
rmyxddbihczskunpfwbjqoeutv
rmyxgdlipczskunofbbjqoeatv
gmyxgdlihczskunpfkbjroeatv
rmyxgdllhcpskunpfwbjqqeatv
rmyxgdlihchskunpfwbjqoelcv
mmyxldlihczskuncfwbjqoeatv
ryyxgdlxhczskcnpfwbjqoeatv
rmyxpdlihczskyntfwbjqoeatv
rmhxgdlibczskwnpfwbjqoeatv
rmyxgdlihczskunpfwojbkeatv
qmyxgdlihczskunpfwbjqoyatm
rmyxgdlzhczskunpfwbjqoealr
rmyegdliqczskunpfgbjqoeatv
umyxgdlihczsvunpfwbfqoeatv
rmyxgdoihfzskunpfmbjqoeatv
rmyxgdlihcdskanpmwbjqoeatv
rmyxgdyihczskunpfrbjqoeaov
rcyxgdlihczskuegfwbjqoeatv
rmyxgdlihgwskunpfwbjkoeatv
rpyxgdlihmzskunpfwbjqoeatp
rmyxgdlihhzskunpfwbjaoeapv
rmyxgdsrhczskunpflbjqoeatv
rmrxgdlihczskunpvwbjqoeabv
rmcxgylihczskunpfwbjyoeatv
rmkxgdlyhczsounpfwbjqoeatv
rmyxgdqihczskunmfwbjqoratv
rmyxgdlihczskunpfibjqofath
rmyxgdliqczskunpqwbjqoeaev
rmhxgdlizcjskunpfwbjqoeatv
rmyxgdlfhcwskunpfwbjqoeaqv
rmyxgdlchclskunpfwbdqoeatv
rmyxgdluhczswunpfwbjqoeatt
rmyxgdlzqczskunpfwbjqoeatq
rmdxgdlihszskunpfwbwqoeatv
rmyxgdlihszsvunpfwbjqueatv
rmyxgdlhhczskunpffbjaoeatv
rmrxgdlphczskunpfwbjqreatv
hmyngdxihczskunpfwbjqoeatv
rmyxgdlizczpkunpfwbyqoeatv
rmyxbdlihyzskunlfwbjqoeatv
rmyxgdlipczsqunnfwbjqoeatv
rmyxgdlihcsskunpfxbjqoaatv
rmyxgdljhcznkunpfwbjqfeatv
rmaxgdlihczspunpfwbjqoqatv
rsyxgdlihczskunpfwbjqoehcv
rmyxgjlicczskunpfwbjqoeitv
rwymgvlihczskunpfwbjqoeatv
rmyxgdlipfzskunpfwbjqweatv
rmyxgglihczskunpgwbjqoealv
royxgdlihczskhnpfwbyqoeatv
rmyxgdlihczskvnpfabkqoeatv
rmyxgdlihczskunpfwhjwzeatv
jlyxgdlihczskunpfwbjqzeatv
rmyxgdlihccskunpfwwjqopatv
rmyxgxlihczskuupfwbjqoeahv
rmyxgdcihcbskungfwbjqoeatv
tmyxgdlihczskunpfwbjmoeftv
rkyxgdlioczskmnpfwbjqoeatv
rmyxgdlrhczskulpfwbjaoeatv
rmysgdlihczikunphwbjqoeatv
rmyxgdlihczskuvpfwbjqoeyty
fmyxgdlihczscunpfqbjqoeatv
rfyxgdlihzzrkunpfwbjqoeatv
rmyxgdlikczskunpfwbjqolath
rmyxqdlihjzskunpfwbjqoeamv
rmuxodiihczskunpfwbjqoeatv
rmyygdliucuskunpfwbjqoeatv
rmyxgdliwczskuppawbjqoeatv
rmyxgdlihczskunprwbjqgehtv
imyvgdlihczskunpfwbjqouatv
rgyxgdluhczskunpflbjqoeatv
rmgxgdlihczsdunpfwwjqoeatv
gdyxgdlihczskunpfwbjqoeavv
rmyxgdlihczskunpfwljjoektv
rmexgdlihczskunpfwxjqoeytv
rmyxqdlihcyskuwpfwbjqoeatv
rmyxgdlihczskunpfiyjqcebtv
amyngdlihczskunpfwbjqseatv
rmzxgdlihczykubpfwbjqoeatv
rmyxgdlihczhkuopfwbjsoeatv
rmyxgdlihczskunpfwbaqowztv
rmgxgdlihczslunpfwbjeoeatv
rmytgdlzhczskunrfwbjqoeatv
rmyxgdtihczskunafobjqoeatv
rmyxgdlihczskuflfbbjqoeatv
rmdxgdlihczskunpfwbjqoealj
rbyxgdlihczskuppdwbjqoeatv
rmyxhdiihcwskunpfwbjqoeatv
rmmggdlfhczskunpfwbjqoeatv
rmbxgblihczskuypfwbjqoeatv
rmyxgslihczsjunpjwbjqoeatv
rmyxgdlohczsaunpfwbjboeatv
rmaxgdhihczskunpfwbjooeatv
rmyxidlihczskunpfgbuqoeatv
rmyxgdlihfzckznpfwbjqoeatv
rmaqgdpihczskunpfwbjqoeatv
rmyvgdlirczskunpfobjqoeatv
rmdxgdlihczlkunpxwbjqoeatv
rmyxgdlihczseunpfwbjvdeatv
rmyxgdlihczskuhpfwbjqneath
rmyxrdlihciskunpfwbjqoratv
rmyxgdmihczsqunpftbjqoeatv
rmyxgdlbhczskulpfbbjqoeatv
rmoxgdlihczskunpfwbjqoeesv
rmyxgdlihczskuijfwejqoeatv
rmyxgdlihczskunpfwnkqoxatv
rmyxgdvihmzskuupfwbjqoeatv
rkyxedlihczskunpfcbjqoeatv
rmyxgdjihczskunprwbjqieatv
omyxgqgihczskunpfwbjqoeatv
rmyxydlihczskunpfwkjqoentv
rmbxgdlicczskunpfwbjqteatv
emyxgdlihczskugpfwbjqneatv
dmyxgflihczskunpfwbjqjeatv
umyxgdlihczskunpfwbjloextv
rmyxgdlihczsbunpfwbyqpeatv
rmyxgdrihczsvunpcwbjqoeatv
qmyxgdlihcwsknnpfwbjqoeatv
ymyxgdlihczskunpfsbjqowatv
rmyxgdlbhczskunpnvbjqoeatv
rmyxfdlixczskunpfwbjqoertv
rmyygdlihszrkunpfwbjqoeatv
rmyxgxlihcpskunpfwbjqoeanv
rmyxgdlihczskjnpfwbjqoprtv
rmyxgdlisczfkunpfwbjqoeath
rmyxgdlihczskunpfkbjqoeaji
rmyxgylihczskunpfwbfqoeatl
rmsxgdbihczskunpfwtjqoeatv
smyxgdlihczskunpfwbjqcwatv
rmyxgdlihczskunppjljqoeatv
rmyxgdlihczskulpfdbjooeatv
rmyxgdlihczskunpfibjqcebtv
rmyxadlihczskunpgwbjyoeatv
rmyxgdlihczdkunpvwbjqoeytv
rmyxgdlihcvskunpfwbjxohatv
rmyxgplihczskunpfgbjqoeauv
rmyxgdlihcysrunmfwbjqoeatv
rmyygdlihczskunpfwbjqvewtv
rmyxgdlihczsmunpfwdjnoeatv
rmyxgdbibczskunpfwbjuoeatv
rmyfgdlihczskubpfwbjqoeatp
rmyxgdlihczskuopfzijqoeatv
rmyqgdlihczskunpwwbjqoeanv
imyxgdlihczskunpfwbjqoqytv
rmyxgdlixcoskbnpfwbjqoeatv
rmyxgrlihccskunpfwbjqteatv
rdyxgdlihcpskunpfwbjqoratv
rmyxgdlihkzskunpfwbjmoeatj
rmyxgslihczskcnpfjbjqoeatv
rmyxgdlihczsqunqfwdjqoeatv
rjyxgdlyhczbkunpfwbjqoeatv
rmyxudlihczjkunpfwbjqzeatv
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
