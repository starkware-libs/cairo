use array::ArrayTrait;
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

    assert(input.len() == 1, 'Wrong input length');
    let res = keccak::keccak_uint256s_le(input.span());

    assert(res.low == 0x587f7cc3722e9654ea3963d5fe8c0748, 'Wrong hash value1');
    assert(res.high == 0xa5963aa610cb75ba273817bce5f8c48f, 'Wrong hash value2');

    input.append(u256 { low: 0, high: 0 });
    input.append(u256 { low: 0, high: 0 });
    input.append(u256 { low: 0, high: 0 });

    assert(input.len() == 4, 'Wrong input length');
    let res = keccak::keccak_uint256s_le(input.span());
    assert(res.low == 0x6f9be8a0c55db082f9299494b18c4eae, 'Wrong hash value3');
    assert(res.high == 0xe1d6ca9618d0d91b04f22d9f298f1f7f, 'Wrong hash value4');
}
