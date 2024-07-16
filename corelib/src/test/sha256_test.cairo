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
            0xe3b0c442,
            0x98fc1c14,
            0x9afbf4c8,
            0x996fb924,
            0x27ae41e4,
            0x649b934c,
            0xa495991b,
            0x7852b855
        ]
    );
    // test length 1
    assert_eq!(
        compute_sha256_byte_array(@"v"),
        [
            0x4c94485e,
            0xc21ae6c,
            0x41ce1dfe,
            0x7b6bface,
            0xea5ab68e,
            0x40a2476f,
            0x50208e52,
            0x6f506080
        ]
    );
    // test length 7
    assert_eq!(
        compute_sha256_byte_array(@"krukfbm"),
        [
            0x575b94cf,
            0xdb42a0a1,
            0xd0198538,
            0x327db87a,
            0xa8234e93,
            0xb331dc2f,
            0xcfa3ab68,
            0x7364a099
        ]
    );
    // test length 8
    assert_eq!(
        compute_sha256_byte_array(@"mupnvimd"),
        [
            0xa5a269a3,
            0x12bd601,
            0x53eeddff,
            0x6561a6a2,
            0x73957a56,
            0x8c9525ae,
            0x1ff5b21f,
            0xf62b2d18
        ]
    );
    // test length 9
    assert_eq!(
        compute_sha256_byte_array(@"wdyxastbn"),
        [
            0x699e18ca,
            0x85a8803,
            0x74037e58,
            0xe678dd88,
            0xd9afecfe,
            0x2ba23651,
            0xc82b4cf6,
            0x4bacb3d
        ]
    );
    // test length 15
    assert_eq!(
        compute_sha256_byte_array(@"qflijxrfdlljpsi"),
        [
            0x214be572,
            0xd52c6e3a,
            0x8a06cb1c,
            0xa1fa50e6,
            0x34f9be5f,
            0x889cde4f,
            0x8968c3e3,
            0x75553dee
        ]
    );
    // test length 16
    assert_eq!(
        compute_sha256_byte_array(@"dioqrlmutkpvtxmf"),
        [
            0x61cb7c00,
            0xe909d153,
            0xda8f85b5,
            0x4ecee95c,
            0x2e6ab310,
            0xf1b5dc77,
            0xf711390c,
            0x31dae697
        ]
    );
    // test length 17
    assert_eq!(
        compute_sha256_byte_array(@"ujamtiddwujswhuwo"),
        [
            0x54a8a022,
            0x397fb6ab,
            0x375e07d,
            0x7e3d499e,
            0x5ed95751,
            0xc21efc94,
            0x38c35f1f,
            0x3ddfdc00
        ]
    );
    // test length 23
    assert_eq!(
        compute_sha256_byte_array(@"pzxbwqoqpipieoxnitjmniq"),
        [
            0x552173d8,
            0xd48312b8,
            0xa67cac79,
            0xcd30faef,
            0x50ee19fd,
            0x8bf6668a,
            0x6f1593a1,
            0xcf2f35cc
        ]
    );
    // test length 24
    assert_eq!(
        compute_sha256_byte_array(@"btqalowoigoajyphnhlkvpkf"),
        [
            0x176fdc9a,
            0xb90cdac0,
            0x51e8e6e3,
            0x130ddf9,
            0x2382e0e4,
            0x9472cb7b,
            0xf9fb88db,
            0x52a939eb
        ]
    );
    // test length 25
    assert_eq!(
        compute_sha256_byte_array(@"yctkoqkzvxznblmqkrbvxsyos"),
        [
            0xb09922b8,
            0x4832398b,
            0xd93eef8,
            0x3eb0a703,
            0x5efff0b4,
            0x6c2334dc,
            0x124866ed,
            0xe9591b6d
        ]
    );
    // test length 31
    assert_eq!(
        compute_sha256_byte_array(@"nfaadsikyqxedtzssejlsmdcugmcjnd"),
        [
            0xc385ffe3,
            0xa1e566d,
            0xd9051a61,
            0x9d1104c3,
            0xe118860f,
            0x66449207,
            0x3325e7a,
            0x545d7d08
        ]
    );
    // test length 32
    assert_eq!(
        compute_sha256_byte_array(@"camyuiskectzsfatfykxqepvcsveacgn"),
        [
            0xdebc312a,
            0xa970b78b,
            0xefc54329,
            0xcafc276f,
            0xc938e739,
            0xb0a4c1df,
            0xf44c4009,
            0xbeba0211
        ]
    );
    // test length 33
    assert_eq!(
        compute_sha256_byte_array(@"rmvisvvxkxaorwqdclfdqblgbyeaevffh"),
        [
            0x27a6a22,
            0x74992689,
            0x6c22f929,
            0x4e6dd8bb,
            0x43c0e0cb,
            0xc6c5f7fb,
            0x84a8d60d,
            0x5d979cd7
        ]
    );
    // test length 39
    assert_eq!(
        compute_sha256_byte_array(@"cmrmtkrabhwoyvnzftdvbtshdykkmawkyduudjd"),
        [
            0xe16a136d,
            0xd008ec2b,
            0xdf32b8f6,
            0x4a341a06,
            0x7f1ddac4,
            0xdf3123fd,
            0xbc303658,
            0x324c95fb
        ]
    );
    // test length 40
    assert_eq!(
        compute_sha256_byte_array(@"uvbjdwynouufjfqsfenedukfytrxndykgmboexhv"),
        [
            0xc514c519,
            0x9e71379d,
            0xd723948d,
            0x7bcd2209,
            0xc149ea4d,
            0x5a0585ca,
            0xfbca9fb5,
            0x49980e0d
        ]
    );
    // test length 41
    assert_eq!(
        compute_sha256_byte_array(@"cfwdhsdwdwecjndqmacydyhashcyhrkdhjscfpxwh"),
        [
            0x87cd2644,
            0xf64d4f96,
            0xa0ab428e,
            0x807ac440,
            0x3f694527,
            0xd39eb648,
            0x42cb633e,
            0x1537bd1
        ]
    );
    // test length 47
    assert_eq!(
        compute_sha256_byte_array(@"zavrmnczvrjeergucimdwepvyraweoqzxtvmfxsnjqiacnt"),
        [
            0x100b78fd,
            0xda7329cb,
            0x19d44baf,
            0x71768f2f,
            0x33c1b434,
            0xaa0260de,
            0xc4e19cb7,
            0x667718bf
        ]
    );
    // test length 48
    assert_eq!(
        compute_sha256_byte_array(@"dblvmzwneclfdwqzhrgkxvfsplbbaisvjlziezciqvbffedh"),
        [
            0x5eec4234,
            0xe07672c2,
            0x6b9eefb6,
            0x8a521e55,
            0xf66b73c9,
            0xf46f08f2,
            0xce021986,
            0x966f68e9
        ]
    );
    // test length 49
    assert_eq!(
        compute_sha256_byte_array(@"nhhsngplzqgspbyojwrcdbgfgxydcjpcmxgppuirrlxhubdmy"),
        [
            0x56e3882a,
            0xd1273be5,
            0xc8b08b7d,
            0x57b351cb,
            0x481de98e,
            0xbf9cdcfc,
            0x11a2af0e,
            0xee1cd7b3
        ]
    );
    // test length 55
    assert_eq!(
        compute_sha256_byte_array(@"xdqzwexlmkfyjolgtqexgboqzxpgurkzrodlwiwiuaxuactflnkvaaz"),
        [
            0xd52fb5ac,
            0x17b20fa0,
            0xb5c80bf7,
            0x727fc4fb,
            0xca0f1480,
            0xcfdc669b,
            0xb37aecc2,
            0x9144ec68
        ]
    );
    // test length 56
    assert_eq!(
        compute_sha256_byte_array(@"sjoubtobmymctfcjzgpzixzljqfohtfpuzziqlwyztbcamavwcapnuzy"),
        [
            0x61071480,
            0xa118f56b,
            0x6ec6bc03,
            0x39af07e9,
            0x9e3b6b9a,
            0x70c9bb64,
            0x38e1ccad,
            0xab732129
        ]
    );
    // test length 57
    assert_eq!(
        compute_sha256_byte_array(@"iyhuqbkawilyisohmxsqfejhkahgnbsbfrcxpfipknxassdccljxugnqn"),
        [
            0xa3d49981,
            0x24d792f2,
            0xe1cd27d4,
            0x9ca3a5d3,
            0xaa955f30,
            0x7b1b960f,
            0x927d1e9a,
            0x9831307
        ]
    );
    // test length 63
    assert_eq!(
        compute_sha256_byte_array(
            @"uqnclpoxluxxjltvumciivhfktiqqjwrrmfvyiscrywwetniowobrevzftuggtg"
        ),
        [
            0xc412ed81,
            0x815e1b55,
            0x95f5abfc,
            0x26f7eaec,
            0xffef1858,
            0x71989ef5,
            0xb8b6345a,
            0xa8b54d5d
        ]
    );
    // test length 64
    assert_eq!(
        compute_sha256_byte_array(
            @"isrxibwjozpodfhqdrfvnrpskgsnnjwbpyijepmpargniuroissvmljzcxndwjmi"
        ),
        [
            0xbee4ff64,
            0x4dec53d1,
            0x780d4157,
            0xefa3652e,
            0x5595f3bc,
            0x99b76484,
            0xe2f0076e,
            0x5bbe7ff0
        ]
    );
    // test length 65
    assert_eq!(
        compute_sha256_byte_array(
            @"algdbhapybdqqhgstsrxtyykrbxbrvpzvtlrqvtyiqkqbsmwlxzeiiboospwdmvnd"
        ),
        [
            0x3bf6f9bd,
            0x33f3e9ee,
            0xc798d27b,
            0x52b48ea5,
            0x80fc9c63,
            0xf68d684f,
            0xfcc0e919,
            0x51c97bf4
        ]
    );
    // test length 71
    assert_eq!(
        compute_sha256_byte_array(
            @"vtxmaocbghjjhdmocpnmmqnhshtxgmlujvoddiinuclvloqigmfaqjqtjkxounbjfamfeik"
        ),
        [
            0xcc8b1906,
            0x82e32558,
            0x4a51af06,
            0xf85830b2,
            0xc69e005b,
            0xe26bc586,
            0x8dc5c388,
            0x674b5f08
        ]
    );
    // test length 72
    assert_eq!(
        compute_sha256_byte_array(
            @"agkkvfgwewvmdcaodwitqpdyccwlvoclflerqosptvslsawlnerarcxllrhpcnhqpsdakoah"
        ),
        [
            0xaf0dd8a5,
            0x16839959,
            0xf591e7b4,
            0x270e952d,
            0x48b6d7a5,
            0x1699121a,
            0x36af67f7,
            0xd712873f
        ]
    );
    // test length 73
    assert_eq!(
        compute_sha256_byte_array(
            @"xjdnayiydtbavvpxzyaxkjmucuoatsiuvhbxjlpietqymphgjrfaehgukiyftoamaemvhkowk"
        ),
        [
            0x93a8d49b,
            0xe771fbbe,
            0x55c69bfd,
            0x668f5a68,
            0x92091491,
            0xbce3293b,
            0x899b5937,
            0xc0e10e34
        ]
    );
}
