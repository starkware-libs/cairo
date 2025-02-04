use crate::sha256::compute_sha256_byte_array;

#[test]
fn test_sha256_byte_array() {
    assert_eq!(
        sha256_as_u256("a"), 0xca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb,
    );
    assert_eq!(
        sha256_as_u256("aa"), 0x961b6dd3ede3cb8ecbaacbd68de040cd78eb2ed5889130cceb4c49268ea4d506,
    );
    assert_eq!(
        sha256_as_u256("aaa"), 0x9834876dcfb05cb167a5c24953eba58c4ac89b1adf57f28f2f9d09af107ee8f0,
    );
    assert_eq!(
        sha256_as_u256("aaaa"), 0x61be55a8e2f6b4e172338bddf184d6dbee29c98853e0a0485ecee7f27b9af0b4,
    );
    // test length 0
    assert_eq!(
        sha256_as_u256(""), 0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855,
    );
    // test length 1
    assert_eq!(
        sha256_as_u256("x"), 0x2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881,
    );
    // test length 7
    assert_eq!(
        sha256_as_u256("xffidcw"),
        0x692ca8f4448048d3eee81365d91f9220c4c446d123bf9dcd82a6e4d0dfd0940a,
    );
    // test length 8
    assert_eq!(
        sha256_as_u256("szhgvskg"),
        0xbea98c851b81bb113b5b8fbb7add06a8aaae36555b63bbf3abe393b8b35260bd,
    );
    // test length 9
    assert_eq!(
        sha256_as_u256("nokwxzwsl"),
        0x063dc8cf320d4dd4403c2e22b7255ab21898c97e4a131d26e1fc0589f7e9cbc4,
    );
    // test length 15
    assert_eq!(
        sha256_as_u256("bhmvtgdkhgajqaf"),
        0xaffa8850bb7b05ac9e948fda305bfe39a15057a0269225ae8c2eeeb3c46e8fe8,
    );
    // test length 16
    assert_eq!(
        sha256_as_u256("qxwwcxzjiibvkqky"),
        0xef1b40920adc9bd453103e171e79776be73f53288dd22baeb087f664e300d1e2,
    );
    // test length 17
    assert_eq!(
        sha256_as_u256("ipjjkvuojtcinvvnu"),
        0x5419df507a08e45a805ae8dcdcfe544fbdfb955e71b65b048f46b4961aebf06c,
    );
    // test length 23
    assert_eq!(
        sha256_as_u256("ygjbndjbjqfqwrywlpggahh"),
        0xb717c1690433f7e07809147d740c2061ce62ca12ac1e458bb2c9c7b4e720e1e2,
    );
    // test length 24
    assert_eq!(
        sha256_as_u256("tqeruqfqlvrsxhjjzyfiuyni"),
        0xb4706bbfe2551f3838df517953f1f2e4cbd869c020fc2a6a78bd6dce26b2996d,
    );
    // test length 25
    assert_eq!(
        sha256_as_u256("oppudxpkzsspqtmopvdhicyhc"),
        0xd81c9f032105f08d7411af404b9c40ba773e9fb5109ec43d742ab2b124b0985f,
    );
    // test length 31
    assert_eq!(
        sha256_as_u256("iehpuxzsxzffqyhqxsnsybpygdscgrf"),
        0x0814d4d5d16c637c0c32b9cbe4831284027ea275f4363c7a0c2006eefcaf6a95,
    );
    // test length 32
    assert_eq!(
        sha256_as_u256("dwdnlgtqkicjpyonhxtzqmxxqsjiwuov"),
        0x41e8c2ec97ab4c2ca0ea63a4d457d184f9daeeecfc67369fb485657227ee4055,
    );
    // test length 33
    assert_eq!(
        sha256_as_u256("owhqocwxqapkjjryeveiuvyomnzgptexp"),
        0x7ca52baf871a67553eae8e1b7e319a4186f41a31bea6a8b13553986b5494304d,
    );
    // test length 39
    assert_eq!(
        sha256_as_u256("jhgdvhubfsgmvqaiehclbhxtkerusriurmepeyz"),
        0x7ca8143c46fb792674f225441b7471b7f545571517478bce80e7e0c9cdbefff8,
    );
    // test length 40
    assert_eq!(
        sha256_as_u256("pbzqxiqewdfzvelghpjclwwhkocssdmknssasdap"),
        0xdeff2a399ce47b6289bd6c051341403e195b4a68c299729414d13bcdef3eaee3,
    );
    // test length 41
    assert_eq!(
        sha256_as_u256("twxzmlfepsjutlpmlpsetkfvixlttvegfdxeefcda"),
        0xebd7f16ac58ae7cc6fa5469c229b896f2a76440d394e0717ff1c49cc777dd81b,
    );
    // test length 47
    assert_eq!(
        sha256_as_u256("ahdlyumrwxiwumrvkscepnxtptzsdkzhlfpuhypdoeqcqql"),
        0x31db2dea1937ece94652015558e98da557a1c6cf1841cd25e3e0748818a75f1c,
    );
    // test length 48
    assert_eq!(
        sha256_as_u256("kmjeneddmhlcsyokduzjovbynzatmmqoekslfwiqbcoaudgd"),
        0x0b9cda0453fa1adbe558a18f7af8c72090891471ac3b09faa1ddf942d223c73b,
    );
    // test length 49
    assert_eq!(
        sha256_as_u256("vyqcddnljnjajyilmafspxnwgxlgwulpkdgvpaweitfgjcgue"),
        0x1ea01ee2976b0361f8a24346c0f26911a98597a9ab2ebbd293fe7cfad72e6a7a,
    );
    // test length 55
    assert_eq!(
        sha256_as_u256("wugdolcxgjgjugcibpydicnpjkeygkowbkqvsqhnydknzpiguhcwaxt"),
        0xb3fffae87e85a7d9085d04210e29a3dfc27469c61e60a957ad0c87db71c8cb79,
    );
    // test length 56
    assert_eq!(
        sha256_as_u256("sdkxocmqvmljrezvkkfnylvjoklmpeyimvtnhtkheylolizwtqulbluf"),
        0x5c2b04d3b63a9fd04afe1137a00f1c642a962933e20ef1d4e4729fd1cadb01a6,
    );
    // test length 57
    assert_eq!(
        sha256_as_u256("msrqhpytgzqnavnazvhswjwifxwvkiapcetishwuagxultoimyfzoself"),
        0x55bc8a6602d12fc6d5354ad9964e2d13ca9e3ede3e2e788c4c928d75b175a7ca,
    );
    // test length 63
    assert_eq!(
        sha256_as_u256("xtoesagtsybcibtwsqxqltbdiygasnozjniwqnjakjogmcvdpujpprgcdmtuyvn"),
        0x7f70e2a9a8854f0634b57721b48fa5f95185b2ca1099d9e093088e416245da06,
    );
    // test length 64
    assert_eq!(
        sha256_as_u256("fjxootkrqqljdfdvwwxjhndsqshphzaoehyyibqulmtihoutvofsekymjwczelit"),
        0x58c09482981dffd40109b7c14c82418ac1841395e9bbf106d53b0f228b37a464,
    );
    // test length 65
    assert_eq!(
        sha256_as_u256("phtpnnjkxyinhxkwklyvxxrmhzaenpkauucubsubwcjucuyauxahxvylffqiqezbd"),
        0x9cba0ff0b0a2ec07208b46ef0a89a27bbbfe23fd24d8b9939a603c2f47bac1b0,
    );
    // test length 71
    assert_eq!(
        sha256_as_u256("gjgdfsbmbbyhbuujhwlsvobhsfooaogvzxeixzpccpzogkkkkxuorhtaaemyojknipfdewb"),
        0x7d3bc3e84c1a82e4ea1e16a5150f73657a4d54f1ee127bd963f6ecf160c6d3bb,
    );
    // test length 72
    assert_eq!(
        sha256_as_u256("kbziqjtltnzicqhvcgiqfbvamdhddiqzyqucuzbualvrajlcaslsvubilzfwvioysvdqypcp"),
        0xd1e18df0c2acc467e21cd7c7ea46c239bfdf3b75cc55addd255eee751daf5d54,
    );
    // test length 73
    assert_eq!(
        sha256_as_u256("ugombvhpwrkpgvqdwjpopdbqvmldlupczklkdevzkhsjzfylgrkotaoltbnxtoqdhposxtuaz"),
        0xe64839141b5b17df5f84c7fb5561723f6050cf0bc52c81082a50f2de99e4a617,
    );
}

/// computes the sha256 of the input and returns it as a u256.
fn sha256_as_u256(input: ByteArray) -> u256 {
    let hash_result = compute_sha256_byte_array(@input);
    let mut value: u256 = 0;
    for word in hash_result.span() {
        value *= 0x100000000;
        value = value + (*word).into();
    }
    value
}

