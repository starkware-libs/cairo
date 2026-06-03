use crate::blake::{blake2s_compress, blake2s_finalize, blake2s_finalize_guarantees};
use crate::hash::{HashStateExTrait, HashStateTrait};
use crate::poseidon::PoseidonTrait;
use crate::test::test_utils::assert_eq;

#[test]
fn test_pedersen_hash() {
    assert_eq(
        @crate::pedersen::pedersen(1, 2),
        @2592987851775965742543459319508348457290966253241455514226127639100457844774,
        'Wrong hash value',
    );
}

#[test]
fn test_poseidon_hades_permutation() {
    let (s0, s1, s2) = crate::poseidon::hades_permutation(1, 2, 3);
    assert_eq(
        @s0,
        @442682200349489646213731521593476982257703159825582578145778919623645026501,
        'wrong s0',
    );
    assert_eq(
        @s1,
        @2233832504250924383748553933071188903279928981104663696710686541536735838182,
        'wrong s1',
    );
    assert_eq(
        @s2,
        @2512222140811166287287541003826449032093371832913959128171347018667852712082,
        'wrong s2',
    );
}

#[test]
fn test_poseidon_hash_span() {
    // Test odd number of inputs.
    assert_eq!(
        crate::poseidon::poseidon_hash_span([1, 2, 3].span()),
        0x2f0d8840bcf3bc629598d8a6cc80cb7c0d9e52d93dab244bbf9cd0dca0ad082,
    );

    // Test even number of inputs.
    assert_eq!(
        crate::poseidon::poseidon_hash_span([1, 2, 3, 4].span()),
        0x26e3ad8b876e02bc8a4fc43dad40a8f81a6384083cabffa190bcf40d512ae1d,
    );
}

#[derive(Hash)]
enum EnumForHash {
    First,
    Second: felt252,
    Third: (felt252, felt252),
}

#[derive(Hash)]
struct StructForHash {
    first: (),
    second: felt252,
    third: (felt252, felt252),
}

#[test]
fn test_user_defined_hash() {
    assert_eq(
        @PoseidonTrait::new().update_with(EnumForHash::First).finalize(),
        @PoseidonTrait::new().update(0).finalize(),
        'Bad hash of EnumForHash::First',
    );
    assert_eq(
        @PoseidonTrait::new().update_with(EnumForHash::Second(5)).finalize(),
        @PoseidonTrait::new().update(1).update(5).finalize(),
        'Bad hash of EnumForHash::Second',
    );
    assert_eq(
        @PoseidonTrait::new().update_with(EnumForHash::Third((6, 8))).finalize(),
        @PoseidonTrait::new().update(2).update(6).update(8).finalize(),
        'Bad hash of EnumForHash::Third',
    );
    assert_eq(
        @PoseidonTrait::new()
            .update_with(StructForHash { first: (), second: 10, third: (6, 17) })
            .finalize(),
        @PoseidonTrait::new().update(10).update(6).update(17).finalize(),
        'Bad hash of StructForHash',
    );
}


#[test]
fn test_blake2s() {
    let state = BoxTrait::new([0_u32; 8]);
    let msg = BoxTrait::new([0_u32; 16]);
    let byte_count = 64_u32;
    assert_eq!(
        to_u256(blake2s_compress(state, byte_count, msg)),
        0x2ae416e8d875987d552ca6fd49f4c6a211f67aca29762fdd2323d9bc3babc315,
    );
    assert_eq!(
        to_u256(blake2s_finalize(state, byte_count, msg)),
        0x593a50789b4b8567eb33bbea06bec58d3d5532f8879cd2640c714de2e373f3e,
    );
}

#[test]
fn test_blake2s_with_abc() {
    // hashing `abc` as it is done in RFC 7693 Appendix B.
    // Initial state is the IV, with keylen 0 and output length 32.
    let state = BoxTrait::new(
        [
            0x6A09E667 ^ (0x01010000 ^ 0x20), 0xBB67AE85, 0x3C6EF372, 0xA54FF53A, 0x510E527F,
            0x9B05688C, 0x1F83D9AB, 0x5BE0CD19,
        ],
    );
    // Message `abc` padded with zeros.
    let msg = BoxTrait::new(['cba', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
    assert_eq!(
        to_u256(blake2s_finalize(state, 3, msg)),
        0x508c5e8c327c14e2e1a72ba34eeb452f37458b209ed63a294d999b4c86675982,
    );
}

#[test]
fn test_blake2s_split_and_guarantees() {
    // hashing `abc` as it is done in RFC 7693 Appendix B.
    // Initial state is the IV, with keylen 0 and output length 32.
    let state = BoxTrait::new(
        [
            0x6A09E667 ^ (0x01010000 ^ 0x20), 0xBB67AE85, 0x3C6EF372, 0xA54FF53A, 0x510E527F,
            0x9B05688C, 0x1F83D9AB, 0x5BE0CD19,
        ],
    );
    assert_eq!(
        to_u256(blake2s_finalize_guarantees(state, 3, msg::from_felt252s('cba', 0))),
        0x508c5e8c327c14e2e1a72ba34eeb452f37458b209ed63a294d999b4c86675982,
    );
    assert_eq!(
        to_u256(
            blake2s_finalize_guarantees(
                state, 32, msg::from_felt252s('\x0543210zyxwvutsrqponmlkjihgfedcba', 0),
            ),
        ),
        0x39b7197928a66cd232d8c5b74d02215a21386228e772076eaf544395b5d32c03,
    );
}

fn to_u256(value: Box<[u32; 8]>) -> u256 {
    let mut result: u256 = 0;
    for word in value.unbox().span() {
        result *= 0x100000000;
        result += (0x1000000 * (*word % 0x100)).into();
        result += (0x10000 * (*word / 0x100 % 0x100)).into();
        result += (0x100 * (*word / 0x10000 % 0x100)).into();
        result += (*word / 0x1000000 % 0x100).into();
    }
    result
}

mod msg {
    #[feature("bounded-int-utils")]
    type U32Guarantee =
        core::internal::bounded_int::BoundedIntGuarantee<0, 0xffffffff>;
    pub extern fn u128_to_u32_guarantees(
        value: u128,
    ) -> (U32Guarantee, U32Guarantee, U32Guarantee, U32Guarantee) nopanic;

    pub fn from_felt252s(a: felt252, b: felt252) -> Box<[U32Guarantee; 16]> {
        let a: u256 = a.into();
        let b: u256 = b.into();
        let (a_w0, a_w1, a_w2, a_w3) = u128_to_u32_guarantees(a.low);
        let (a_w4, a_w5, a_w6, a_w7) = u128_to_u32_guarantees(a.high);
        let (b_w0, b_w1, b_w2, b_w3) = u128_to_u32_guarantees(b.low);
        let (b_w4, b_w5, b_w6, b_w7) = u128_to_u32_guarantees(b.high);
        BoxTrait::new(
            [
                a_w0, a_w1, a_w2, a_w3, a_w4, a_w5, a_w6, a_w7, b_w0, b_w1, b_w2, b_w3, b_w4, b_w5,
                b_w6, b_w7,
            ],
        )
    }
}
