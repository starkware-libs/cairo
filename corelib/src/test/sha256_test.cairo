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
}
