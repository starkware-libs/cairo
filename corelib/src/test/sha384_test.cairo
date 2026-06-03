use crate::sha384::{compute_sha384_byte_array, compute_sha384_u64_array};

#[test]
fn test_sha384_byte_array() {
    // docstring examples
    assert_eq!(
        compute_sha384_byte_array(@"Hello world"),
        [
            0x9203b0c4439fd1e6, 0xae5878866337b7c5, 0x32acd6d9260150c8, 0x0318e8ab8c27ce33,
            0x0189f8df94fb890d, 0xf1d298ff360627e1,
        ],
    );
    assert_eq!(
        compute_sha384_byte_array(@"Hello world"),
        compute_sha384_u64_array(array![0x48656c6c6f20776f], 0x726c64, 3),
    );
    // test length 0
    assert_eq!(
        compute_sha384_byte_array(@""),
        [
            0x38b060a751ac9638, 0x4cd9327eb1b1e36a, 0x21fdb71114be0743, 0x4c0cc7bf63f6e1da,
            0x274edebfe76f65fb, 0xd51ad2f14898b95b,
        ],
    );
    // test length 3 ("abc" — NIST FIPS 180-4 known answer)
    assert_eq!(
        compute_sha384_byte_array(@"abc"),
        [
            0xcb00753f45a35e8b, 0xb5a03d699ac65007, 0x272c32ab0eded163, 0x1a8b605a43ff5bed,
            0x8086072ba1e7cc23, 0x58baeca134c825a7,
        ],
    );
    // test length 5
    assert_eq!(
        compute_sha384_byte_array(@"Hello"),
        [
            0x3519fe5ad2c596ef, 0xe3e276a6f351b8fc, 0x0b03db861782490d, 0x45f7598ebd0ab5fd,
            0x5520ed102f38c4a5, 0xec834e98668035fc,
        ],
    );
    // test length 7
    assert_eq!(
        compute_sha384_byte_array(@"xffidcw"),
        [
            0x8004d8e41704e1ba, 0x90dd7e4394acd0d4, 0xe18f8d90a52280f3, 0x17ae31024afb4481,
            0xc0a13e96483fb5cf, 0x1ae8d9d5fc0dbf28,
        ],
    );
    // test length 8
    assert_eq!(
        compute_sha384_byte_array(@"szhgvskg"),
        [
            0x1620bc53aa63ae0f, 0x94f12067555b2e04, 0xa2b2c6d1d48abc20, 0x564882a13bfe3ebf,
            0x98508234002a98cf, 0x9c6c5621d764afec,
        ],
    );
    // test length 9
    assert_eq!(
        compute_sha384_byte_array(@"nokwxzwsl"),
        [
            0x09caee8ea56924ad, 0x4b27f96a9d22c0db, 0xf26ab9cea075bbc6, 0x848a9ea7324cb08e,
            0x53dfaac4c4fc927a, 0xe2e6e92e08f0bd5d,
        ],
    );
    // test length 15
    assert_eq!(
        compute_sha384_byte_array(@"bhmvtgdkhgajqaf"),
        [
            0x29197f1097c99ae9, 0x770903aca2a0a602, 0xe15c40ed6de2268c, 0x2553f1dc34148ee7,
            0xecfbce6e1010ac7f, 0x826bb2de8f86d9ff,
        ],
    );
    // test length 16
    assert_eq!(
        compute_sha384_byte_array(@"qxwwcxzjiibvkqky"),
        [
            0xfa4bc74d58001242, 0x5c629c33b42bc487, 0xdfd2dfb465541161, 0xdf08cc68eb725d97,
            0xa23098ffecbfa5a5, 0x54d95609df87692d,
        ],
    );
    // test length 111 (last byte fits in one block)
    assert_eq!(
        compute_sha384_byte_array(
            @"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        ),
        [
            0x3c37955051cb5c30, 0x26f94d551d5b5e2a, 0xc38d572ae4e07172, 0x085fed81f8466b8f,
            0x90dc23a8ffcdea0b, 0x8d8e58e8fdacc80a,
        ],
    );
    // test length 112 (requires a second block)
    assert_eq!(
        compute_sha384_byte_array(
            @"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        ),
        [
            0x187d4e07cb306103, 0xc69967bf544d0dfb, 0xe9042577599c73c3, 0x30abc0cb64c61236,
            0xd5ed565ee19119d8, 0xc31779a38f791fcd,
        ],
    );
    // test length 128 (exactly two blocks)
    assert_eq!(
        compute_sha384_byte_array(
            @"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        ),
        [
            0xedb12730a366098b, 0x3b2beac75a3bef1b, 0x0969b15c48e2163c, 0x23d96994f8d1bef7,
            0x60c7e27f3c464d38, 0x29f56c0d53808b0b,
        ],
    );
}
