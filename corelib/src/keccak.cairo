use array::Span;
use array::ArrayTrait;
use array::SpanTrait;
use array::ArrayDrop;
use integer::TryInto;
use option::OptionTrait;
use starknet::SyscallResultTrait;

const KECCAK_FULL_RATE_IN_U64S: usize = 17;


fn u128_to_u64(input: u128) -> u64 {
    input.try_into().unwrap()
}

fn u128_split(input: u128) -> (u64, u64) {
    let (high, low) = integer::u128_safe_divmod(
        input, integer::u128_try_as_non_zero(0x10000000000000000).unwrap()
    );

    (u128_to_u64(high), u128_to_u64(low))
}


// Computes the keccak256 of multiple little-endian uint256 values.
fn keccak_uint256s_le(mut input: Span<u256>) -> u256 {
    let mut keccak_input: Array::<u64> = ArrayTrait::new();

    loop {
        match input.pop_front() {
            Option::Some(v) => {
                let (high, low) = u128_split(*v.low);
                keccak_input.append(low);
                keccak_input.append(high);
                let (high, low) = u128_split(*v.high);
                keccak_input.append(low);
                keccak_input.append(high);
            },
            Option::None(_) => {
                break ();
            },
        };
    };

    add_padding(ref keccak_input);
    starknet::syscalls::keccak_syscall(keccak_input.span()).unwrap_syscall()
}


// The padding in keccak256 is 10*1;
fn add_padding(ref input: Array<u64>) {
    let divisor = integer::u32_try_as_non_zero(KECCAK_FULL_RATE_IN_U64S).unwrap();
    let (q, r) = integer::u32_safe_divmod(input.len(), divisor);
    let padding_len = KECCAK_FULL_RATE_IN_U64S - r;
    // padding_len is in the range [1, KECCAK_FULL_RATE_IN_U64S].

    if padding_len == 1 {
        input.append(0x8000000000000001);
        return ();
    }

    // padding_len >= 2;
    input.append(1);
    finalize_padding(ref input, padding_len - 1);
}

// Finalize the padding by appending 0*1.
fn finalize_padding(ref input: Array<u64>, padding_len: u32) {
    if (padding_len == 1) {
        input.append(0x8000000000000000);
        return ();
    }

    input.append(0);
    finalize_padding(ref input, padding_len - 1);
}
