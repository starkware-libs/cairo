use starknet::testing::cheatcode;
use array::ArrayTrait;
use array::SpanTrait;

#[test]
#[available_gas(300000)]
fn test_set_block_number() {
    let mut data = ArrayTrait::new();
    data.append(42);

    let r = cheatcode::<'print'>(data.span());

    assert(1 == 1, '');
}
