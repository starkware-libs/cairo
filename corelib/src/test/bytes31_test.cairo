use test::test_utils::assert_eq;
use option::OptionTrait;
use traits::{Into, TryInto};

const POW_2_248: felt252 = 0x100000000000000000000000000000000000000000000000000000000000000;

#[test]
fn test_bytes31_casts() {
    let zero_as_bytes31: Option<bytes31> = 0.try_into();
    assert(zero_as_bytes31.is_some(), '0 is not a bytes31');
    let zero_as_felt252 = zero_as_bytes31.unwrap().into();
    assert(zero_as_felt252 == 0_felt252, 'bad casts: 0');

    let one_as_bytes31: Option<bytes31> = 1.try_into();
    assert(one_as_bytes31.is_some(), '1 is not a bytes31');
    let one_as_felt252 = one_as_bytes31.unwrap().into();
    assert(one_as_felt252 == 1_felt252, 'bad casts: 1');

    let max_as_bytes31: Option<bytes31> = (POW_2_248 - 1).try_into();
    assert(max_as_bytes31.is_some(), '2^248 - 1 is not a bytes31');
    let max_as_felt252 = max_as_bytes31.unwrap().into();
    assert(max_as_felt252 == POW_2_248 - 1, 'bad casts: 2^248 - 1');

    let out_of_range: Option<bytes31> = POW_2_248.try_into();
    assert(out_of_range.is_none(), '2^248 is a bytes31');

    let out_of_range: Option<bytes31> = (-1).try_into();
    assert(out_of_range.is_none(), '-1 is a bytes31');
}
