use crate::sha512::compute_sha512_byte_array;

#[test]
fn test_sha512_byte_array() {
    // length 0
    assert_eq!(
        compute_sha512_byte_array(@""),
        [
            0xcf83e1357eefb8bd, 0xf1542850d66d8007, 0xd620e4050b5715dc, 0x83f4a921d36ce9ce,
            0x47d0d13c5d85f2b0, 0xff8318d2877eec2f, 0x63b931bd47417a81, 0xa538327af927da3e,
        ],
    );
    // length 3
    assert_eq!(
        compute_sha512_byte_array(@"abc"),
        [
            0xddaf35a193617aba, 0xcc417349ae204131, 0x12e6fa4e89a97ea2, 0x0a9eeee64b55d39a,
            0x2192992a274fc1a8, 0x36ba3c23a3feebbd, 0x454d4423643ce80e, 0x2a9ac94fa54ca49f,
        ],
    );
    // length 5
    assert_eq!(
        compute_sha512_byte_array(@"Hello"),
        [
            0x3615f80c9d293ed7, 0x402687f94b22d58e, 0x529b8cc7916f8fac, 0x7fddf7fbd5af4cf7,
            0x77d3d795a7a00a16, 0xbf7e7f3fb9561ee9, 0xbaae480da9fe7a18, 0x769e71886b03f315,
        ],
    );
    // length 7 (last u64 word has 7 valid bytes)
    assert_eq!(
        compute_sha512_byte_array(@"abcdefg"),
        [
            0xd716a4188569b68a, 0xb1b6dfac178e5701, 0x14cdf0ea3a1cc0e3, 0x1486c3e41241bc6a,
            0x76424e8c37ab26f0, 0x96fc85ef9886c8cb, 0x634187f4fddff645, 0xfb099f1ff54c6b8c,
        ],
    );
    // length 8 (exactly one u64 word)
    assert_eq!(
        compute_sha512_byte_array(@"abcdefgh"),
        [
            0xa3a8c81bc97c2560, 0x010d7389bc88aac9, 0x74a104e0e2381220, 0xc6e084c4dccd1d2d,
            0x17d4f86db31c2a85, 0x1dc80e6681d74733, 0xc55dcd03dd96f606, 0x2cdda12a291ae6ce,
        ],
    );
    // length 9 (one full word + 1 trailing byte)
    assert_eq!(
        compute_sha512_byte_array(@"abcdefghi"),
        [
            0xf22d51d25292ca1d, 0x0f68f69aedc78970, 0x19308cc9db46efb7, 0x5a03dd494fc7f126,
            0xc010e8ade6a00a0c, 0x1a5f1b75d81e0ed5, 0xa93ce98dc9b833db, 0x7839247b1d9c24fe,
        ],
    );
    // length 15
    assert_eq!(
        compute_sha512_byte_array(@"abcdefghijklmno"),
        [
            0xdb723f341a042d8d, 0xe1aa813efd5e02fc, 0x1745ccbe25948625, 0x7514804e2ec4bceb,
            0xb2a46f1e4ad44215, 0x4943f9e97e1bc47c, 0x3ae0eddab7de0c01, 0xa9c51f15342a5b19,
        ],
    );
    // length 16 (exactly two u64 words)
    assert_eq!(
        compute_sha512_byte_array(@"abcdefghijklmnop"),
        [
            0xd0cadd6834fa0c15, 0x7b36cca30ee8b0b1, 0x435d841aa5b5ac85, 0x0c11ae80a1440f51,
            0x743e98fb1f1e7376, 0xc70f2f65404f088c, 0x28bcb4a511df2e64, 0x111f8f7424364b60,
        ],
    );
    // length 111 (one byte before the boundary requiring a second padding block)
    assert_eq!(
        compute_sha512_byte_array(@"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"),
        [
            0xfa9121c7b32b9e01, 0x733d034cfc78cbf6, 0x7f926c7ed83e8220, 0x0ef8681819692176,
            0x0b4beff48404df81, 0x1b95382827446167, 0x3c68d04e297b0eb7, 0xb2b4d60fc6b566a2,
        ],
    );
    // length 112 (exactly at the boundary — requires a second block for padding)
    assert_eq!(
        compute_sha512_byte_array(@"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"),
        [
            0xc01d080efd492776, 0xa1c43bd23dd99d0a, 0x2e626d481e16782e, 0x75d54c2503b5dc32,
            0xbd05f0f1ba33e568, 0xb88fd2d970929b71, 0x9ecbb152f58f130a, 0x407c8830604b70ca,
        ],
    );
    // length 128 (exactly two blocks of input)
    assert_eq!(
        compute_sha512_byte_array(@"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"),
        [
            0xb73d1929aa615934, 0xe61a871596b3f3b3, 0x3359f42b8175602e, 0x89f7e06e5f658a24,
            0x3667807ed300314b, 0x95cacdd579f3e33a, 0xbdfbe351909519a8, 0x46d465c59582f321,
        ],
    );
}
