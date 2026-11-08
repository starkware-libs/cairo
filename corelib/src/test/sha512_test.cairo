use crate::sha512::{compute_sha512_byte_array, compute_sha512_u64_array};

#[test]
fn test_sha512_byte_array() {
    // docstring examples
    assert_eq!(
        compute_sha512_byte_array(@"Hello world"),
        [
            0xb7f783baed8297f0, 0xdb917462184ff4f0, 0x8e69c2d5e5f79a94, 0x2600f9725f58ce1f,
            0x29c18139bf80b06c, 0x0fff2bdd34738452, 0xecf40c488c22a7e3, 0xd80cdf6f9c1c0d47,
        ],
    );
    assert_eq!(
        compute_sha512_byte_array(@"Hello world"),
        compute_sha512_u64_array(array![0x48656c6c6f20776f], 0x726c64, 3),
    );
    // test length 0
    assert_eq!(
        compute_sha512_byte_array(@""),
        [
            0xcf83e1357eefb8bd, 0xf1542850d66d8007, 0xd620e4050b5715dc, 0x83f4a921d36ce9ce,
            0x47d0d13c5d85f2b0, 0xff8318d2877eec2f, 0x63b931bd47417a81, 0xa538327af927da3e,
        ],
    );
    // test length 3 ("abc" — NIST FIPS 180-4 known answer)
    assert_eq!(
        compute_sha512_byte_array(@"abc"),
        [
            0xddaf35a193617aba, 0xcc417349ae204131, 0x12e6fa4e89a97ea2, 0x0a9eeee64b55d39a,
            0x2192992a274fc1a8, 0x36ba3c23a3feebbd, 0x454d4423643ce80e, 0x2a9ac94fa54ca49f,
        ],
    );
    // test length 5
    assert_eq!(
        compute_sha512_byte_array(@"Hello"),
        [
            0x3615f80c9d293ed7, 0x402687f94b22d58e, 0x529b8cc7916f8fac, 0x7fddf7fbd5af4cf7,
            0x77d3d795a7a00a16, 0xbf7e7f3fb9561ee9, 0xbaae480da9fe7a18, 0x769e71886b03f315,
        ],
    );
    // test length 7
    assert_eq!(
        compute_sha512_byte_array(@"xffidcw"),
        [
            0x53ccff917edc643b, 0x7fbed8e5bab9bca4, 0x04fc4328c31cc941, 0xc6b403e0365fe542,
            0x9bd6a44e66aa77da, 0xab8a0820ef5fd5b8, 0xd0095c775865ce28, 0xd0fb057c8fe0a701,
        ],
    );
    // test length 8
    assert_eq!(
        compute_sha512_byte_array(@"szhgvskg"),
        [
            0x4197b9e27c90c2a9, 0xa3d90023ef91bd39, 0x848e68d5cae5ff40, 0x3a99a4df6c6c8e10,
            0xb5388b01f9b672b0, 0x49c514e4017df911, 0x2e0c81e059fc22de, 0x967354c26bb3e8d1,
        ],
    );
    // test length 9
    assert_eq!(
        compute_sha512_byte_array(@"nokwxzwsl"),
        [
            0x4916a813bea33b5a, 0xc64517c1e52ab1a8, 0xbe5602d378d6517c, 0xaf5a4acc76f0d67f,
            0xeee699efda7fde4b, 0x2d98c587c0dfa9b7, 0x6a1fa89b410d7d38, 0x7d675e87ba45022f,
        ],
    );
    // test length 15
    assert_eq!(
        compute_sha512_byte_array(@"bhmvtgdkhgajqaf"),
        [
            0x285e30b168ec8c7c, 0x3a43e59ab1be93c7, 0x985b9c5e02b391aa, 0x1334050d668a4491,
            0x9f92508829626456, 0xe1a43fb4bb8fed1a, 0xa2654dc2e5ba8cd3, 0x4718965803203768,
        ],
    );
    // test length 16
    assert_eq!(
        compute_sha512_byte_array(@"qxwwcxzjiibvkqky"),
        [
            0x3b066424eab2880b, 0xeb76ecf9d1797163, 0x99e6ed072aac3a12, 0xf3f02c8f62715352,
            0x541ce9728962a0db, 0xdc6066d35c04c0a9, 0x389ea3b53051e74a, 0x0aacaa7c1c13cb80,
        ],
    );
    // test length 111 (last byte fits in one block)
    assert_eq!(
        compute_sha512_byte_array(
            @"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        ),
        [
            0xfa9121c7b32b9e01, 0x733d034cfc78cbf6, 0x7f926c7ed83e8220, 0x0ef8681819692176,
            0x0b4beff48404df81, 0x1b95382827446167, 0x3c68d04e297b0eb7, 0xb2b4d60fc6b566a2,
        ],
    );
    // test length 112 (requires a second block)
    assert_eq!(
        compute_sha512_byte_array(
            @"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        ),
        [
            0xc01d080efd492776, 0xa1c43bd23dd99d0a, 0x2e626d481e16782e, 0x75d54c2503b5dc32,
            0xbd05f0f1ba33e568, 0xb88fd2d970929b71, 0x9ecbb152f58f130a, 0x407c8830604b70ca,
        ],
    );
    // test length 128 (exactly two blocks)
    assert_eq!(
        compute_sha512_byte_array(
            @"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        ),
        [
            0xb73d1929aa615934, 0xe61a871596b3f3b3, 0x3359f42b8175602e, 0x89f7e06e5f658a24,
            0x3667807ed300314b, 0x95cacdd579f3e33a, 0xbdfbe351909519a8, 0x46d465c59582f321,
        ],
    );
}
