use array::ArrayTrait;
use array::ArrayDefault;
use core::traits::Into;
use core::traits::Default;
use starknet::SyscallResultTrait;


#[test]
#[available_gas(10000000)]
fn test_keccak_syscall() {
    let mut input = ArrayDefault::default();
    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });

    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });

    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });

    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });

    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });
    input.append(u256 { low: 1, high: 0 });

    let res = keccak::keccak_u256s_le_inputs(input.span());
    assert(res.low == 0xec2b33c45859c58e55785c610cd02aac, res.low.into());
    assert(res.high == 296576420271519180506110224534327831208, res.high.into());
}
