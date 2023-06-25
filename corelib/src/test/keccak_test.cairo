use array::ArrayTrait;
use core::traits::Into;
use starknet::SyscallResultTrait;
use test::test_utils::{assert_eq, assert_ne};

#[test]
#[available_gas(100000)]
fn test_keccak_syscall() {
    let mut input = array![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17];
    assert_eq(
        @starknet::syscalls::keccak_syscall(input.span()).unwrap_syscall(),
        @u256 { low: 0xec687be9c50d2218388da73622e8fdd5, high: 0xd2eb808dfba4703c528d145dfe6571af },
        'Wrong hash value'
    );
}

#[test]
#[available_gas(10000000)]
fn test_keccak_hash() {
    let res = keccak::keccak_u256s_le_inputs(array![1].span());
    assert_eq(@res.low, @0x587f7cc3722e9654ea3963d5fe8c0748, 'Wrong hash value1');
    assert_eq(@res.high, @0xa5963aa610cb75ba273817bce5f8c48f, 'Wrong hash value2');

    let res = keccak::keccak_u256s_be_inputs(array![1].span());
    assert_eq(@res.low, @0x326a7e71fdcdee263b071276522d0eb1, 'Wrong hash value3');
    assert_eq(@res.high, @0xf60cfab7e2cb9f2d73b0c2fa4a4bf40c, 'Wrong hash value4');

    let res = keccak::keccak_u256s_le_inputs(array![1, 2, 3, 4].span());
    assert_eq(@res.low, @0x845f8e9f5191367fb5181e74f6eb550d, 'Wrong hash value5');
    assert_eq(@res.high, @0x17a2126cf7391a26b41c36a687090cc5, 'Wrong hash value6');

    let res = keccak::keccak_u256s_be_inputs(array![1, 2, 3, 4].span());
    assert_eq(@res.low, @0x6510e6fd534f267a01086462df912739, 'Wrong hash value7');
    assert_eq(@res.high, @0x2d9982dfaf468a9ddf7101b6323aa9d5, 'Wrong hash value8');
}
