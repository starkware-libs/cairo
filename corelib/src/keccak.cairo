//! Implementation of the Keccak-256 cryptographic hash function.
//!
//! Four functions are made available for computing Keccak-256 hash:
//! - `keccak_u256s_le_inputs`: Computes the Keccak-256 hash of multiple `u256` little-endian
//! values.
//! - `keccak_u256s_be_inputs`: Computes the Keccak-256 hash of multiple `u256` big-endian values.
//! - `cairo_keccak`: Computes the Keccak-256 hash of `input` + `last_input_num_bytes` LSB bytes of
//! `last_input_word`.
//! - `compute_keccak_byte_array`: Computes the Keccak-256 hash of a `ByteArray` input.
//!
//! # Examples
//!
//! ```
//! use core::keccak::{keccak_u256s_le_inputs, keccak_u256s_be_inputs, cairo_keccak};
//!
//! let input: Span<u256> = array![0, 1, 2].span();
//! let hash = keccak_u256s_le_inputs(input);
//! let hash = keccak_u256s_be_inputs(input);
//!
//! let mut input: Array<u64> = array![1, 2, 3];
//! let hash = cairo_keccak(ref input, 0, 0);
//!
//! let mut input: ByteArray = "input";
//! let hash = compute_keccak_byte_array(@input);
//! ```

use crate::array::{Span, ArrayTrait, SpanTrait};
use crate::traits::TryInto;
use crate::option::OptionTrait;
use crate::starknet::SyscallResultTrait;

const KECCAK_FULL_RATE_IN_BYTES: usize = 136;
const KECCAK_FULL_RATE_IN_U64S: usize = 17;
const BYTES_IN_U64_WORD: usize = 8;


fn u128_to_u64(input: u128) -> u64 {
    input.try_into().unwrap()
}

fn u128_split(input: u128) -> (u64, u64) {
    let (high, low) = crate::integer::u128_safe_divmod(
        input, 0x10000000000000000_u128.try_into().unwrap(),
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

/// Computes the Keccak-256 hash of multiple `u256` values.
/// The input values are interpreted as little-endian.
/// The 32-byte result is represented as a little-endian `u256`.
///
/// # Examples
///
/// ```
/// use core::keccak::keccak_u256s_le_inputs;
///
/// let input: Span<u256> = array![0, 1, 2].span();
/// let hash = keccak_u256s_le_inputs(input);
/// assert!(hash == 108564409375760768785839210880094122205681344913968620748694289447820501098662);
/// ```
pub fn keccak_u256s_le_inputs(mut input: Span<u256>) -> u256 {
    let mut keccak_input: Array::<u64> = Default::default();

    loop {
        match input.pop_front() {
            Option::Some(v) => { keccak_add_u256_le(ref keccak_input, *v); },
            Option::None => { break (); },
        };
    };

    add_padding(ref keccak_input, 0, 0);
    starknet::syscalls::keccak_syscall(keccak_input.span()).unwrap_syscall()
}

fn keccak_add_u256_be(ref keccak_input: Array::<u64>, v: u256) {
    let (high, low) = u128_split(crate::integer::u128_byte_reverse(v.high));
    keccak_input.append(low);
    keccak_input.append(high);
    let (high, low) = u128_split(crate::integer::u128_byte_reverse(v.low));
    keccak_input.append(low);
    keccak_input.append(high);
}

/// Computes the Keccak-256 hash of multiple `u256` values.
/// The input values are interpreted as big-endian.
/// The 32-byte result is represented as a little-endian `u256`.
///
/// # Examples
///
/// ```
/// use core::keccak::keccak_u256s_be_inputs;
///
/// let input: Span<u256> = array![0, 1, 2].span();
/// let hash = keccak_u256s_be_inputs(input);
/// assert!(hash == 570847462879755027369133508877705016900393103153136337402584556374429500134);
/// ```
pub fn keccak_u256s_be_inputs(mut input: Span<u256>) -> u256 {
    let mut keccak_input: Array::<u64> = Default::default();

    loop {
        match input.pop_front() {
            Option::Some(v) => { keccak_add_u256_be(ref keccak_input, *v); },
            Option::None => { break (); },
        };
    };

    add_padding(ref keccak_input, 0, 0);
    starknet::syscalls::keccak_syscall(keccak_input.span()).unwrap_syscall()
}

/// Computes the Keccak-256 hash of `input` + `last_input_num_bytes` LSB bytes of `last_input_word`.
/// To use this function, split the input into words of 64 bits (little endian).
/// For example, to compute keccak('Hello world!'), use:
///   inputs = [8031924123371070792, 560229490]
/// where:
///   8031924123371070792 == int.from_bytes(b'Hello wo', 'little')
///   560229490 == int.from_bytes(b'rld!', 'little')
///
/// Returns the hash as a little endian `u256`.
///
/// # Examples
///
/// ```
/// use core::keccak::cairo_keccak;
///
/// let mut input: Array<u64> = array![1, 2, 3];
/// let hash = cairo_keccak(ref input, 0, 0);
/// assert!(hash == 6252579295546323668400833151898997979548922124224871075182053169822751592236);
/// ```
pub fn cairo_keccak(
    ref input: Array<u64>, last_input_word: u64, last_input_num_bytes: usize,
) -> u256 {
    add_padding(ref input, last_input_word, last_input_num_bytes);
    starknet::syscalls::keccak_syscall(input.span()).unwrap_syscall()
}

/// The padding in Keccak-256 is "1 0* 1".
/// `last_input_num_bytes` (0-7) is the number of bytes in the last u64 input - `last_input_word`.
fn add_padding(ref input: Array<u64>, last_input_word: u64, last_input_num_bytes: usize) {
    let words_divisor = KECCAK_FULL_RATE_IN_U64S.try_into().unwrap();
    // `last_block_num_full_words` is in range [0, KECCAK_FULL_RATE_IN_U64S - 1]
    let (_, last_block_num_full_words) = crate::integer::u32_safe_divmod(
        input.len(), words_divisor,
    );

    // The first word to append would be of the form
    //     0x1<`last_input_num_bytes` LSB bytes of `last_input_word`>.
    // For example, for `last_input_num_bytes == 4`:
    //     0x1000000 + (last_input_word & 0xffffff)
    let first_word_to_append = if last_input_num_bytes == 0 {
        // This case is handled separately to avoid unnecessary computations.
        1
    } else {
        let first_padding_byte_part = if last_input_num_bytes == 1 {
            0x100
        } else if last_input_num_bytes == 2 {
            0x10000
        } else if last_input_num_bytes == 3 {
            0x1000000
        } else if last_input_num_bytes == 4 {
            0x100000000
        } else if last_input_num_bytes == 5 {
            0x10000000000
        } else if last_input_num_bytes == 6 {
            0x1000000000000
        } else if last_input_num_bytes == 7 {
            0x100000000000000
        } else {
            crate::panic_with_felt252('Keccak last input word >7b')
        };
        let (_, r) = crate::integer::u64_safe_divmod(
            last_input_word, first_padding_byte_part.try_into().unwrap(),
        );
        first_padding_byte_part + r
    };

    if last_block_num_full_words == KECCAK_FULL_RATE_IN_U64S - 1 {
        input.append(0x8000000000000000 + first_word_to_append);
        return;
    }

    // last_block_num_full_words < KECCAK_FULL_RATE_IN_U64S - 1
    input.append(first_word_to_append);
    finalize_padding(ref input, KECCAK_FULL_RATE_IN_U64S - 1 - last_block_num_full_words);
}

/// Finalizes the padding by appending "0* 1".
fn finalize_padding(ref input: Array<u64>, num_padding_words: u32) {
    if (num_padding_words == 1) {
        input.append(0x8000000000000000);
        return;
    }

    input.append(0);
    finalize_padding(ref input, num_padding_words - 1);
}

/// Computes the Keccak-256 hash of the input `ByteArray` and returns the hash as a little endian
/// `u256`.
///
/// # Examples
///
/// ```
/// let mut input: ByteArray = "input";
/// let hash = compute_keccak_byte_array(@input);
/// assert!(hash == 5011455638164454593316177716808989569815817790756088890569531890924147099028);
/// ```
pub fn compute_keccak_byte_array(arr: @ByteArray) -> u256 {
    let mut input = array![];
    let mut i = 0;
    let mut inner = 0;
    let mut limb: u64 = 0;
    let mut factor: u64 = 1;
    while let Option::Some(b) = arr.at(i) {
        limb = limb + b.into() * factor;
        i += 1;
        inner += 1;
        if inner == 8 {
            input.append(limb);
            inner = 0;
            limb = 0;
            factor = 1;
        } else {
            factor *= 0x100;
        }
    };
    add_padding(ref input, limb, inner);
    starknet::syscalls::keccak_syscall(input.span()).unwrap_syscall()
}
