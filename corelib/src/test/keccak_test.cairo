use array::ArrayTrait;
use starknet::SyscallResultTrait;
use debug::PrintTrait;

#[test]
#[available_gas(100000)]
fn test_keccak_hash() {
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
            high: 0x528d145dfe6571afd2eb808dfba4703c, low: 0x388da73622e8fdd5ec687be9c50d2218
        },
        'Wrong hash value'
    );
}
