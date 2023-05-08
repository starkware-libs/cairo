use array::ArrayTrait;
use core::traits::Into;
use starknet::SyscallResultTrait;

#[test]
#[available_gas(100000)]
fn test_keccak_syscall() {
    let mut input = ArrayTrait::new();
    input.append(1);
    input.append(2);
    input.append(3);
    input.append(4);
    input.append(5);
    input.append(6);
    input.append(7);
    input.append(8);
    input.append(9);
    input.append(10);
    input.append(11);
    input.append(12);
    input.append(13);
    input.append(14);
    input.append(15);
    input.append(16);
    input.append(17);
    assert(
        starknet::syscalls::keccak_syscall(input.span()).unwrap_syscall() == u256 {
            low: 0xec687be9c50d2218388da73622e8fdd5, high: 0xd2eb808dfba4703c528d145dfe6571af
        },
        'Wrong hash value'
    );
}

#[test]
#[available_gas(10000000)]
fn test_keccak_hash() {
    let mut input = ArrayTrait::new();
    input.append(u256 { low: 1, high: 0 });

    let res = keccak::keccak_uint256s_le(input.span());

    assert(res.low == 0x587f7cc3722e9654ea3963d5fe8c0748, 'Wrong hash value1');
    assert(res.high == 0xa5963aa610cb75ba273817bce5f8c48f, 'Wrong hash value2');

    let res = keccak::keccak_uint256s_be(input.span());

    assert(res.low == 0x326a7e71fdcdee263b071276522d0eb1, 'Wrong hash value3');
    assert(res.high == 0xf60cfab7e2cb9f2d73b0c2fa4a4bf40c, 'Wrong hash value4');

    input.append(u256 { low: 2, high: 0 });
    input.append(u256 { low: 3, high: 0 });
    input.append(u256 { low: 4, high: 0 });

    let res = keccak::keccak_uint256s_le(input.span());
    assert(res.low == 0x845f8e9f5191367fb5181e74f6eb550d, 'Wrong hash value5');
    assert(res.high == 0x17a2126cf7391a26b41c36a687090cc5, 'Wrong hash value6');

    let res = keccak::keccak_uint256s_be(input.span());
    assert(res.low == 0x6510e6fd534f267a01086462df912739, 'Wrong hash value7');
    assert(res.high == 0x2d9982dfaf468a9ddf7101b6323aa9d5, 'Wrong hash value8');
}
