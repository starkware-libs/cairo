use core::test::test_utils::{assert_eq, assert_ne};
use core::starknet::SyscallResultTrait;
use core::sha256::compute_sha256_byte_array;
#[test]
fn test_sha256_byte_array() {
    // TODO(TomerStarkware): compare all values when PartialEq is implemented for fixed size array.

    let [a, _, _, _, _, _, _, _,] = compute_sha256_byte_array(@"a");
    assert_eq!(a, 0xca978112);
    let [a, _, _, _, _, _, _, _,] = compute_sha256_byte_array(@"aa");
    assert_eq!(a, 0x961b6dd3);
    let [a, _, _, _, _, _, _, _,] = compute_sha256_byte_array(@"aaa");
    assert_eq!(a, 0x9834876d);
    let [a, _, _, _, _, _, _, _,] = compute_sha256_byte_array(@"aaaa");
    assert_eq!(a, 0x61be55a8);
    let [a, _, _, _, _, _, _, _,] = compute_sha256_byte_array(
        @"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
    );
    assert_eq!(a, 0x06f9b1a7);
    // test length 0
    assert_eq!(
        compute_sha256_byte_array(@""),
        [
            3820012610,
            2566659092,
            2600203464,
            2574235940,
            665731556,
            1687917388,
            2761267483,
            2018687061
        ]
    );
    // test length 1
    assert_eq!(
        compute_sha256_byte_array(@"k"),
        [
            2186593065,
            2837991670,
            3577339191,
            1866995438,
            660885885,
            2782929749,
            347030321,
            1682443642
        ]
    );
    // test length 7
    assert_eq!(
        compute_sha256_byte_array(@"plbtrbb"),
        [846144104, 1483086157, 1739323533, 3247750929, 626531013, 289281010, 3091636864, 123634566]
    );
    // test length 8
    assert_eq!(
        compute_sha256_byte_array(@"hudvmuha"),
        [
            1989129366,
            226334788,
            949496874,
            2379996888,
            3242641920,
            1979699624,
            3710017660,
            3664044219
        ]
    );
    // test length 9
    assert_eq!(
        compute_sha256_byte_array(@"fggycjvfu"),
        [
            4079572187,
            331492728,
            4137120731,
            705670187,
            586039433,
            4088382743,
            2213249745,
            3465654900
        ]
    );
    // test length 15
    assert_eq!(
        compute_sha256_byte_array(@"pbpnjxtvymwjsyp"),
        [
            1775620706,
            1604052610,
            2389054447,
            1205973595,
            1246724171,
            983127468,
            876909171,
            1285807869
        ]
    );
    // test length 16
    assert_eq!(
        compute_sha256_byte_array(@"eiesgpmiccqiagwe"),
        [
            3379022222,
            3339326689,
            3083404277,
            452281695,
            645820183,
            392592109,
            1667661400,
            2799125503
        ]
    );
    // test length 17
    assert_eq!(
        compute_sha256_byte_array(@"evuhoeeqcnnsoixhz"),
        [
            483603388,
            1137233813,
            1229524601,
            3325210692,
            2868222636,
            4018501523,
            1588548674,
            1907151847
        ]
    );
    // test length 23
    assert_eq!(
        compute_sha256_byte_array(@"wfjdkykappzxevdjulmmsbe"),
        [
            3275598448,
            250702484,
            2311341615,
            1772871179,
            1794273988,
            311540829,
            444414122,
            2172097440
        ]
    );
    // test length 24
    assert_eq!(
        compute_sha256_byte_array(@"solrjipgakfqlwxxkdntbqlf"),
        [
            444776728,
            3545101312,
            3294014792,
            2706381028,
            887686809,
            4217953219,
            2036033203,
            566037729
        ]
    );
    // test length 25
    assert_eq!(
        compute_sha256_byte_array(@"zdffotgxbkpuqxnmdvfxepsig"),
        [
            3501780786,
            980432346,
            793464202,
            2800038259,
            1550871664,
            2589441986,
            2434918394,
            1687409569
        ]
    );
    // test length 31
    assert_eq!(
        compute_sha256_byte_array(@"vcbnukoewbrsjoreuaguqnpqcqbsolk"),
        [
            1704933940,
            2144431834,
            2437826802,
            3002410755,
            3060664810,
            2330105803,
            691777297,
            2802883377
        ]
    );
    // test length 32
    assert_eq!(
        compute_sha256_byte_array(@"tsgxvracbylygmcbjcpewujizqpgcxsj"),
        [
            3787255114,
            2355608360,
            1175189145,
            2566886989,
            1447884020,
            1123779155,
            912222702,
            2811244774
        ]
    );
    // test length 33
    assert_eq!(
        compute_sha256_byte_array(@"cvhirbbzkwtkkgedfuqsfqfqsdjnhotcf"),
        [
            2272142910,
            3967146817,
            2156633566,
            4090002660,
            2318524429,
            1083997238,
            3889518142,
            2187165747
        ]
    );
    // test length 39
    assert_eq!(
        compute_sha256_byte_array(@"immrlgyacnhhwdkummlbpohcwthjcypjhgigrqz"),
        [1380442065, 576307628, 77642890, 1476573450, 483909982, 1354440900, 2806006913, 4098034314]
    );
    // test length 40
    assert_eq!(
        compute_sha256_byte_array(@"mqtvioyevryrptuaqrnvfkdpwxjwybvoedttgzhx"),
        [
            2818752876,
            3405141435,
            1127638979,
            1444354128,
            4250201331,
            1596448120,
            2630942314,
            2829037416
        ]
    );
    // test length 41
    assert_eq!(
        compute_sha256_byte_array(@"izsqykhzbtjyictemdusgkopeakxbclrssolizaod"),
        [
            734103717,
            3036359978,
            3293579608,
            3514288099,
            3734784757,
            99985206,
            4233982626,
            2740481558
        ]
    );
    // test length 47
    assert_eq!(
        compute_sha256_byte_array(@"wdnbbfkxlhdjtjryautpzdgffejpezxzljgajfdqfmnrnel"),
        [
            1246360121,
            1129321218,
            1723378828,
            934012150,
            1457210167,
            3863652541,
            2406366471,
            3990886790
        ]
    );
    // test length 48
    assert_eq!(
        compute_sha256_byte_array(@"cwbzemaoqklryqxqkdgtmolnrjwrkdrljginumgyddfkouiz"),
        [203111549, 1709341521, 1123677454, 3440825333, 2938649697, 185343387, 1270771660, 5413661]
    );
    // test length 49
    assert_eq!(
        compute_sha256_byte_array(@"gipmudiecxcydejwexdlsvylohhbdcaudroforryfnbbddrtn"),
        [
            1341222058,
            1450769083,
            3397162052,
            1830904897,
            3645406441,
            3343322299,
            2062716249,
            635015920
        ]
    );
    // test length 55
    assert_eq!(
        compute_sha256_byte_array(@"mqqhbvswcjeqvkznjcimbqlfqowawqosjmsadxpdzfstlkahucixtik"),
        [
            2825301538,
            4073457651,
            2796108846,
            2901793885,
            2053287299,
            668274674,
            2651629422,
            2866000108
        ]
    );
    // test length 56
    assert_eq!(
        compute_sha256_byte_array(@"jyeokgniorvmyspjjvaakutkptxpjezznsucfsbhpuzbadunalceelka"),
        [
            3941263232,
            1208344132,
            716004748,
            1803427688,
            2230985338,
            1165133567,
            2477092018,
            1851365589
        ]
    );
    // test length 57
    assert_eq!(
        compute_sha256_byte_array(@"rwckksoblyvtdxkqsqahqhfvqhdkyaaqricskwhvebdkofxxnkldrnqzn"),
        [
            1977106905,
            3576207320,
            2060434713,
            595409259,
            223040080,
            1938616778,
            3347455731,
            4070876147
        ]
    );
    // test length 63
    assert_eq!(
        compute_sha256_byte_array(
            @"mgijonrreztemvwildgwadpjgzqrduajqiogxjziwawlpljimszzmncpkwflmil"
        ),
        [
            1680291098,
            2739126982,
            1557116807,
            29365068,
            820245572,
            1284049259,
            4079590390,
            1250285244
        ]
    );
    // test length 64
    assert_eq!(
        compute_sha256_byte_array(
            @"fnbywmhgjphvtkuphfuwnvdhjizmszdhvzbaqvaedlwpepiquhgsrvgwuwmsmiyr"
        ),
        [
            3144815847,
            3806517257,
            2678856901,
            2291036460,
            3864354857,
            2691995216,
            1172982415,
            1937089058
        ]
    );
    // test length 65
    assert_eq!(
        compute_sha256_byte_array(
            @"lvuqwquwcdobmvqvfondakjjaxsjsintcrzaqpifscvqxbnxrvvhgahdcgqwdicvj"
        ),
        [
            3347221197,
            128198879,
            1117070849,
            2635784303,
            3671401945,
            1114319361,
            136841178,
            2563609532
        ]
    );
    // test length 71
    assert_eq!(
        compute_sha256_byte_array(
            @"tmejajbwrfnkxwinmmkuaxxhmlzdsqvlrjfsohpblncmrdxlmjxlxfpzohfsbuhqyzcwtmt"
        ),
        [
            101668348,
            4074597201,
            939093820,
            3305749525,
            472130560,
            1365015015,
            3103614776,
            2824637839
        ]
    );
    // test length 72
    assert_eq!(
        compute_sha256_byte_array(
            @"hvgrmzhlwwcgwtqlrbffektvmjqakqzkqngodaewmcmjpborrnkhqzvxcoyjtidnjxqudqla"
        ),
        [
            1910951213,
            3306283242,
            376038151,
            3910881239,
            2627812493,
            169168735,
            820315895,
            1709398901
        ]
    );
    // test length 73
    assert_eq!(
        compute_sha256_byte_array(
            @"zezqwavaillafamhhjwvzxdszadwpcardtvascoouxkbwinykexdqwbwtjfvexshpazhktdat"
        ),
        [
            3545823548,
            3587795446,
            676063049,
            3347034665,
            1546494401,
            3889463563,
            3109547134,
            3732685805
        ]
    );
}
