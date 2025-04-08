//! Keccak-256 cryptographic hash function implementation.
//!
//! # Main Functions
//!
//! - [`keccak_u256s_le_inputs`] - Hash multiple `u256` values in little-endian format
//! - [`keccak_u256s_be_inputs`] - Hash multiple `u256` values in big-endian format
//! - [`cairo_keccak`] - Hash u64 words with a final partial word. Closest to the syscall input.
//! - [`compute_keccak_byte_array`] - Hash a `ByteArray` directly
//!
//! # Examples
//!
//! ```
//! use core::keccak::*;
//!
//! // Hash u256 values
//! let input = array![1_u256, 2_u256].span();
//! assert!(keccak_u256s_le_inputs(input) ==
//! 0x234a9e12e9b063b60f7e3289ee9b86a731de8e7e41bd4987f10982d6a753444d);
//! assert!(keccak_u256s_be_inputs(input) ==
//! 0xe0c2a7d2cc99d544061ac0ccbb083ac8976e54eed878fb1854dfe7b6ce7b0be9);
//!
//! // Hash a `Bytearray`
//! let text: ByteArray = "Hello, Keccak!";
//! assert!(compute_keccak_byte_array(@text) ==
//! 0x85c9aab73219c1e95c5b5966a4ecc8db4418c3500072a830cfb5a2d13d2c2249);
//! ```

use starknet::SyscallResultTrait;
use crate::array::{ArrayTrait, Span, SpanTrait};
use crate::option::OptionTrait;
use crate::traits::TryInto;

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

fn keccak_add_u256_le(ref keccak_input: Array<u64>, v: u256) {
    let (high, low) = u128_split(v.low);
    keccak_input.append(low);
    keccak_input.append(high);
    let (high, low) = u128_split(v.high);
    keccak_input.append(low);
    keccak_input.append(high);
}

/// Computes the Keccak-256 hash of multiple `u256` values in little-endian format.
///
/// # Arguments
///
/// * `input` - A span of little-endian `u256` values to be hashed
///
/// # Returns
///
/// The 32-byte Keccak-256 hash as a little-endian `u256`
///
/// # Examples
///
/// ```
/// use core::keccak::keccak_u256s_le_inputs;
///
/// let input: Span<u256> = array![0, 1, 2].span();
/// assert!(keccak_u256s_le_inputs(input) ==
/// 0xf005473605efc7d8ff67d9f23fe2e4a4f23454c12b49b38822ed362e0a92a0a6);
/// ```
pub fn keccak_u256s_le_inputs(mut input: Span<u256>) -> u256 {
    let mut keccak_input: Array<u64> = Default::default();

    loop {
        match input.pop_front() {
            Some(v) => { keccak_add_u256_le(ref keccak_input, *v); },
            None => { break (); },
        }
    }

    add_padding(ref keccak_input, 0, 0);
    starknet::syscalls::keccak_syscall(keccak_input.span()).unwrap_syscall()
}

fn keccak_add_u256_be(ref keccak_input: Array<u64>, v: u256) {
    let (high, low) = u128_split(crate::integer::u128_byte_reverse(v.high));
    keccak_input.append(low);
    keccak_input.append(high);
    let (high, low) = u128_split(crate::integer::u128_byte_reverse(v.low));
    keccak_input.append(low);
    keccak_input.append(high);
}

/// Computes the Keccak-256 hash of multiple `u256` values in big-endian format.
///
/// # Arguments
///
/// * `input` - A span of big-endian `u256` values to be hashed
///
/// # Returns
///
/// The 32-byte Keccak-256 hash as a little-endian `u256`
///
/// # Examples
///
/// ```
/// use core::keccak::keccak_u256s_be_inputs;
///
/// let input = array![0x1234_u256, 0x5678_u256].span();
/// let hash = assert!(keccak_u256s_be_inputs(input) ==
/// 0xfa31cb2326ed629f79d2da5beb78e2bd8ac7a1b8b86cae09eeb6a89a908b12a);
/// ```
pub fn keccak_u256s_be_inputs(mut input: Span<u256>) -> u256 {
    let mut keccak_input: Array<u64> = Default::default();

    loop {
        match input.pop_front() {
            Some(v) => { keccak_add_u256_be(ref keccak_input, *v); },
            None => { break (); },
        }
    }

    add_padding(ref keccak_input, 0, 0);
    starknet::syscalls::keccak_syscall(keccak_input.span()).unwrap_syscall()
}

/// Computes the Keccak-256 hash of a byte sequence with custom padding.
///
/// This function allows hashing arbitrary byte sequences by providing the input as
/// 64-bit words in little-endian format and a final partial word.
///
/// # Arguments
///
/// * `input` - Array of complete 64-bit words in little-endian format
/// * `last_input_word` - Final partial word (if any)
/// * `last_input_num_bytes` - Number of valid bytes in the final word (0-7)
///
/// # Returns
///
/// The 32-byte Keccak-256 hash as a little-endian `u256`
///
/// # Panics
///
/// Panics if `last_input_num_bytes` is greater than 7.
///
/// # Examples
///
/// ```
/// use core::keccak::cairo_keccak;
///
/// // Hash "Hello world!" by splitting into 64-bit words in little-endian
/// let mut input = array![0x6f77206f6c6c6548]; // a full 8-byte word
/// let hash = cairo_keccak(ref input, 0x21646c72, 4); // 4 bytes of the last word
/// assert!(hash == 0xabea1f2503529a21734e2077c8b584d7bee3f45550c2d2f12a198ea908e1d0ec);
/// ```
pub fn cairo_keccak(
    ref input: Array<u64>, last_input_word: u64, last_input_num_bytes: usize,
) -> u256 {
    add_padding(ref input, last_input_word, last_input_num_bytes);
    starknet::syscalls::keccak_syscall(input.span()).unwrap_syscall()
}

/// Adds Keccak-256 padding according to the "1 0* 1" rule.
///
/// # Arguments
///
/// * `input` - The buffer to pad
/// * `last_input_word` - Final partial word to include before padding
/// * `last_input_num_bytes` - Number of valid bytes in the final word (0-7)
///
/// # Panics
///
/// Panics if `last_input_num_bytes` is greater than 7.
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

/// Computes the Keccak-256 hash of a `ByteArray`.
///
/// # Arguments
///
/// * `arr` - The input bytes to hash
///
/// # Returns
///
/// The 32-byte Keccak-256 hash as a little-endian `u256`
///
/// # Examples
///
/// ```
/// use core::keccak::compute_keccak_byte_array;
///
/// let text: ByteArray = "Hello world!";
/// let hash = compute_keccak_byte_array(@text);
/// assert!(hash == 0xabea1f2503529a21734e2077c8b584d7bee3f45550c2d2f12a198ea908e1d0ec);
/// ```
pub fn compute_keccak_byte_array(arr: @ByteArray) -> u256 {
    let mut input = array![];
    let mut i = 0;
    let mut inner = 0;
    let mut limb: u64 = 0;
    let mut factor: u64 = 1;
    while let Some(b) = arr.at(i) {
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
    }
    add_padding(ref input, limb, inner);
    starknet::syscalls::keccak_syscall(input.span()).unwrap_syscall()
}
