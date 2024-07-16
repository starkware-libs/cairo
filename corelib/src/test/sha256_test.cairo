use core::test::test_utils::{assert_eq, assert_ne};
use core::starknet::SyscallResultTrait;
use core::sha256::compute_sha256_byte_array;
fn to_u256(arr: [u32; 8]) -> u256 {
    let mut value: u256 = 0;
    for word in arr.span() {
        value *= 0x100000000;
        value = value + (*word).into();
    };
    value
}
#[test]
fn test_sha256_byte_array() {
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"a")),
        0xca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb
    );
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"aa")),
        0x961b6dd3ede3cb8ecbaacbd68de040cd78eb2ed5889130cceb4c49268ea4d506
    );
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"aaa")),
        0x9834876dcfb05cb167a5c24953eba58c4ac89b1adf57f28f2f9d09af107ee8f0
    );
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"aaaa")),
        0x61be55a8e2f6b4e172338bddf184d6dbee29c98853e0a0485ecee7f27b9af0b4
    );
    // test length 0
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"")),
        0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
    );
    // test length 1
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"c")),
        0x2e7d2c03a9507ae265ecf5b5356885a53393a2029d241394997265a1a25aefc6
    );
    // test length 7
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"ctkwbfb")),
        0x3347f49b77284d5b85a46ad20b460c7371d12ecfce2e26d505e31139c0013a00
    );
    // test length 8
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"flnhoiaj")),
        0x4f1c486626045bd1a7136c543f3a3940e10b96369ca31fdcc6efb1035cd51d5d
    );
    // test length 9
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"rwmdaqtux")),
        0xece65a05afdb4d2b08c314ae4cf6d11f1aebf9cfd657e48269e0a7312e11dfe9
    );
    // test length 15
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"sqbcdgdtthvdzqt")),
        0x7e68f3a1ec17eb89903575d40629270766f28bcda2e8184e00a7915afca0b21e
    );
    // test length 16
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"opvxjsavmvpssfwf")),
        0xe815c745203bcc190f3fb2abc08de5fbc3814cd080f7aa836cec07a4f224533a
    );
    // test length 17
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"oguwmmvcjqiyyiicx")),
        0x69770224b3d9bace311dc433312d16bcfabb40554e9b04677e1398b1a442fda4
    );
    // test length 23
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"xdwqcgawrqqduqjfjrlbsnn")),
        0x86b9a91be59aef890b949c710e42c4ce47b808b0c8faffdb6b276036b7d98587
    );
    // test length 24
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"hzhfrcqgpsefelbepvqkqvvu")),
        0xdd55028557a7d96f90e5cc450271b1aacbd2d7552a5684c4356694f827b52f5f
    );
    // test length 25
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"mqtgesoksdsninmavfpnnmlre")),
        0xa3b87a16fe42d3b7cd917df6e419dd14589b03e2266270d9d46015030e266c01
    );
    // test length 31
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"soplxfkrcnbwppjfvpomzzxazfwbswp")),
        0xbf3d74b65de5bc36fefd87e3dd8092f5b40adfb3a9c9ae96d96c2dc1b942c237
    );
    // test length 32
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"xhkhqyfzfepwezlitkxpqtctcicqkeoz")),
        0x8ffc1c5f30dd80f581be2fb7824cfea32b1b50601b5b896d62cbc6f163e43473
    );
    // test length 33
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"zgupgvcrxsmgwfiqcnyijgdnazsemecef")),
        0x0a23773cf22f22e46bc5359c43f08e0fcc71ea30ecb2d8a6f65cd3754f820c6f
    );
    // test length 39
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"wtynymtvarxopaweyfblvjbposshfmfxuvransd")),
        0x619f35d0673c1f22b3b10ef1b2e0c74bcfe1480b7cf6d414cef5d5335d04ee1b
    );
    // test length 40
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"oumvbhjtnrmvczrxvojgdbzyjaiiegaagylfppra")),
        0x9589947fc7e52482d143e9894f7985e52fddc02b7feb72c0cf7ce17c8e043bc2
    );
    // test length 41
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"rfvxvfwaihxvrenzpvkerfhmaxqfrlglqfqrrboro")),
        0x0cb8e437597ce86039ba20fb89a7fa53511941a1a3643cb279f55e967ee15514
    );
    // test length 47
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"twukukairepfrqfuskkpojljwobqjlhqfcvxvuxxhzlpiux")),
        0x530cf91f800e63ba9cd472ead2b00d410aa2f5ba2f2a05020e52b2b52a3397bc
    );
    // test length 48
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"ukmqjsuuobulcnigjhjxqucnexfryfnxkzvveaffyrexwrjp")),
        0xe594d44675813617a9ccdd79c4d64e0f2e9c07103e7084cb9f61970f6fbc6f73
    );
    // test length 49
    assert_eq!(
        to_u256(compute_sha256_byte_array(@"ojskpfszavtxijggqqkemgfrxanjuhnoaiwzztnpdotiobist")),
        0x3ff3ea05d99636f741963155b996e4124b80446df192bfcd854fd871c0bb91b8
    );
    // test length 55
    assert_eq!(
        to_u256(
            compute_sha256_byte_array(@"vlhsdojfmsqssdfkpqqxfyacwgzhnbumrhdvafcgxklbrmhhokdualb")
        ),
        0xe9998815b94281bc8fc47ecceadf53366209284f5ff7788e59f4546820a698bc
    );
    // test length 56
    assert_eq!(
        to_u256(
            compute_sha256_byte_array(@"znqtltmyqahecqanylnxxmexzabqhrmnkpjbnsvkybekevmioiguzwgj")
        ),
        0xeca7ed744f78457b41cde61dd9cf3acbeed8d91aa6f11d8a1d8f454032c9651b
    );
    // test length 57
    assert_eq!(
        to_u256(
            compute_sha256_byte_array(@"mdcmsqojjtimhuxbbbwkzdpnkupgcnvkbjsjxakmkkrgwkgngnwuvzyed")
        ),
        0x65457c22604b7e3605e90958696566e852063540383b7c5161d728c40e62579e
    );
    // test length 63
    assert_eq!(
        to_u256(
            compute_sha256_byte_array(
                @"nluaccbfljtknbilyctjschlpzpfhzmspcvardvlrpcxjyqnjazyxoeslxzbyjc"
            )
        ),
        0x1c67e777f0ad9f7eb0f647851b85ac91e458166f2bade4203583e22418348d3c
    );
    // test length 64
    assert_eq!(
        to_u256(
            compute_sha256_byte_array(
                @"lueirfmuiiqdrhtijmfafmeomznxoyzvvsnpmngkqnpsnlwfngchodivfpaltvsv"
            )
        ),
        0x2cc2797c8916660cf3087faf01a1cb86c76cfac61b1c97ef1ad1cfbb7bc77a4a
    );
    // test length 65
    assert_eq!(
        to_u256(
            compute_sha256_byte_array(
                @"mtqfmowolvghncjnguurwuivwxbsvcmjjwapafgcfvmqfnfzvowjxfkcluxdkwref"
            )
        ),
        0x8451b7402fb3bf112844bebd707ab4cc13f4c3a48051ebe01cd6f505ba050b2d
    );
    // test length 71
    assert_eq!(
        to_u256(
            compute_sha256_byte_array(
                @"zvkzfoxdufoonkdorjhggztvjayitqwbwkjurumgokaomdbwcpdqmbakpwvdfsekhksmxxn"
            )
        ),
        0xcd25816e9596e953a722fe9b05caa86aeb874731a07e3886e8814c11124cf0a1
    );
    // test length 72
    assert_eq!(
        to_u256(
            compute_sha256_byte_array(
                @"pudrmvrekqucsxflrsvuonnejbchzhsfsfnbykyxqffltkrufcrgregzoanuvvmnpmgvbpna"
            )
        ),
        0xf70fb429f73c71d37d77c892fe88403aa1cb1215f08050e60aa1dc82f9a43f60
    );
    // test length 73
    assert_eq!(
        to_u256(
            compute_sha256_byte_array(
                @"fyrahdpvuseuqhounerlclzkekhdejpnqwehjyuzflwsbnkxjkofnnjpdrygyurakmhuotmbk"
            )
        ),
        0xc9a89150766792399149682b4325f6492dec8e1ddea9a36bbc93a04beed4243a
    );
}
