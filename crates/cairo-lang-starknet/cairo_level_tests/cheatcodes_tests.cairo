use starknet::info::get_block_number;
use starknet::testing::{set_block_number, cheatcode};
use test::test_utils::{assert_eq, assert_ne};
use box::BoxTrait;

#[test]
#[available_gas(300000)]
fn test_set_block_number() {
    // Regular set_block_number still works
    set_block_number(100);
    let block_number = get_block_number();
    assert(100 == block_number, 'block number has not been set');

    // Cheatcode set_block_number
    let r = cheatcode::<'set_block_number', felt252, felt252>(BoxTrait::new(200));

    // Block number has been set
    let block_number = get_block_number();
    assert(200 == block_number, 'block number has not been set');

    // Output the same box as the input
    assert(200 == r.unbox(), 'unexpected box content');
}
