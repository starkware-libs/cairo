use array::{Span, ArrayTrait, SpanTrait, ArrayDrop};
use integer::TryInto;
use option::OptionTrait;
use starknet::SyscallResultTrait;

const KECCAK_FULL_RATE_IN_BYTES: usize = 136;
const KECCAK_FULL_RATE_IN_U64S: usize = 17;
const BYTES_IN_U64_WORD: usize = 8;


fn u128_to_u64(input: u128) -> u64 {
    input.try_into().unwrap()
}

fn u128_split(input: u128) -> (u64, u64) {
    let (high, low) = integer::u128_safe_divmod(
        input, 0x10000000000000000_u128.try_into().unwrap()
    );

    (u128_to_u64(high), u128_to_u64(low))
}

fn keccak_add_u256_le(ref keccak_input: Array::<u64>, v: u256) {
    let (high, low) = u128_split(v.low);
    keccak_input.append(low);
    keccak_input.append(high);
    let (high, low) = u128_split(v.high);
    keccak_input.append(low);
    keccak_input.append(high);
}


// Computes the keccak256 of multiple u256 values.
// The input values are interpreted as little-endian.
// The 32-byte result is represented as a little-endian u256.
fn keccak_u256s_le_inputs(mut input: Span<u256>) -> u256 {
    let mut keccak_input: Array::<u64> = Default::default();

    loop {
        match input.pop_front() {
            Option::Some(v) => {
                keccak_add_u256_le(ref keccak_input, *v);
            },
            Option::None(_) => {
                break ();
            },
        };
    };

    add_padding(ref keccak_input, 0, 0);
    starknet::syscalls::keccak_syscall(keccak_input.span()).unwrap_syscall()
}

fn keccak_add_u256_be(ref keccak_input: Array::<u64>, v: u256) {
    let (high, low) = u128_split(integer::u128_byte_reverse(v.high));
    keccak_input.append(low);
    keccak_input.append(high);
    let (high, low) = u128_split(integer::u128_byte_reverse(v.low));
    keccak_input.append(low);
    keccak_input.append(high);
}

// Computes the keccak256 of multiple u256 values.
// The input values are interpreted as big-endian.
// The 32-byte result is represented as a little-endian u256.
fn keccak_u256s_be_inputs(mut input: Span<u256>) -> u256 {
    let mut keccak_input: Array::<u64> = Default::default();

    loop {
        match input.pop_front() {
            Option::Some(v) => {
                keccak_add_u256_be(ref keccak_input, *v);
            },
            Option::None(_) => {
                break ();
            },
        };
    };

    add_padding(ref keccak_input, 0, 0);
    starknet::syscalls::keccak_syscall(keccak_input.span()).unwrap_syscall()
}

// Computes the keccak of `input` + `last_input_num_bytes` LSB bytes of `last_input_word`.
// To use this function, split the input into words of 64 bits (little endian).
// For example, to compute keccak('Hello world!'), use:
//   inputs = [8031924123371070792, 560229490]
// where:
//   8031924123371070792 == int.from_bytes(b'Hello wo', 'little')
//   560229490 == int.from_bytes(b'rld!', 'little')
//
// Returns the hash as a little endian u256.
fn cairo_keccak(ref input: Array<u64>, last_input_word: u64, last_input_num_bytes: usize) -> u256 {
    add_padding(ref input, last_input_word, last_input_num_bytes);
    starknet::syscalls::keccak_syscall(input.span()).unwrap_syscall()
}

// The padding in keccak256 is "1 0* 1".
// `last_input_num_bytes` (0-7) is the number of bytes in the last u64 input - `last_input_word`.
fn add_padding(ref input: Array<u64>, last_input_word: u64, last_input_num_bytes: usize) {
    let words_divisor = KECCAK_FULL_RATE_IN_U64S.try_into().unwrap();
    // `last_block_num_full_words` is in range [0, KECCAK_FULL_RATE_IN_U64S - 1]
    let (_, last_block_num_full_words) = integer::u32_safe_divmod(input.len(), words_divisor);
    // `last_block_num_bytes` is in range [0, KECCAK_FULL_RATE_IN_BYTES - 1]
    let last_block_num_bytes = last_block_num_full_words * BYTES_IN_U64_WORD + last_input_num_bytes;

    if last_block_num_full_words == KECCAK_FULL_RATE_IN_U64S - 1 {
        let word_to_append = if last_input_num_bytes == 0 {
            0x8000000000000001
        } else if last_input_num_bytes == 1 {
            0x8000000000000100 + (last_input_word & 0xff)
        } else if last_input_num_bytes == 2 {
            0x8000000000010000 + (last_input_word & 0xffff)
        } else if last_input_num_bytes == 3 {
            0x8000000001000000 + (last_input_word & 0xffffff)
        } else if last_input_num_bytes == 4 {
            0x8000000100000000 + (last_input_word & 0xffffffff)
        } else if last_input_num_bytes == 5 {
            0x8000010000000000 + (last_input_word & 0xffffffffff)
        } else if last_input_num_bytes == 6 {
            0x8001000000000000 + (last_input_word & 0xffffffffffff)
        } else if last_input_num_bytes == 7 {
            0x8100000000000000 + (last_input_word & 0xffffffffffffff)
        } else {
            panic_with_felt252('Keccak last input word >7b')
        };
        input.append(word_to_append);
        return;
    }

    // last_block_num_full_words < KECCAK_FULL_RATE_IN_U64S - 1
    let first_word_to_append = if last_input_num_bytes == 0 {
        1
    } else if last_input_num_bytes == 1 {
        0x100 + (last_input_word & 0xff)
    } else if last_input_num_bytes == 2 {
        0x10000 + (last_input_word & 0xffff)
    } else if last_input_num_bytes == 3 {
        0x1000000 + (last_input_word & 0xffffff)
    } else if last_input_num_bytes == 4 {
        0x100000000 + (last_input_word & 0xffffffff)
    } else if last_input_num_bytes == 5 {
        0x10000000000 + (last_input_word & 0xffffffffff)
    } else if last_input_num_bytes == 6 {
        0x1000000000000 + (last_input_word & 0xffffffffffff)
    } else if last_input_num_bytes == 7 {
        0x100000000000000 + (last_input_word & 0xffffffffffffff)
    } else {
        panic_with_felt252('Keccak last input word >7b')
    };
    input.append(first_word_to_append);

    finalize_padding(ref input, KECCAK_FULL_RATE_IN_U64S - 1 - last_block_num_full_words);
}

// Finalize the padding by appending "0* 1".
fn finalize_padding(ref input: Array<u64>, num_padding_words: u32) {
    if (num_padding_words == 1) {
        input.append(0x8000000000000000);
        return;
    }

    input.append(0);
    finalize_padding(ref input, num_padding_words - 1);
}

