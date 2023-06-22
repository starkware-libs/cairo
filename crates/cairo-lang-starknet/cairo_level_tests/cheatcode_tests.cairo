use starknet::testing::cheatcode;
use starknet::info::get_block_number;

use array::ArrayTrait;
use array::SpanTrait;

#[test]
#[available_gas(300000)]
fn test_set_block_number() {
    let mut data = ArrayTrait::new();
    data.append(42);

    cheatcode::<'set_block_number'>(data.span());

    let block_number = get_block_number();
    assert(42 == block_number, 'block number has not been set');
}
