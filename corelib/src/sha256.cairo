//! Implementation of the SHA-256 cryptographic hash function.
//!
//! This module provides functions to compute SHA-256 hashes of data.
//! The input data can be an array of 32-bit words, or a `ByteArray`.
//!
//! # Examples
//!
//! ```
//! use core::sha256::compute_sha256_byte_array;
//!
//! let data = "Hello";
//! let hash = compute_sha256_byte_array(@data);
//! assert!(hash == [0x185f8db3, 0x2271fe25, 0xf561a6fc, 0x938b2e26, 0x4306ec30, 0x4eda5180,
//! 0x7d17648, 0x26381969]);
//! ```
use starknet::SyscallResultTrait;
use crate::bytes_31::{
    one_shift_left_bytes_u128,
};
/// A handle to the state of a SHA-256 hash.
pub(crate) extern type Sha256StateHandle;

impl Sha256StateHandleCopy of Copy<Sha256StateHandle>;
impl Sha256StateHandleDrop of Drop<Sha256StateHandle>;

/// Initializes a new SHA-256 state handle with the given initial state.
extern fn sha256_state_handle_init(state: Box<[u32; 8]>) -> Sha256StateHandle nopanic;

/// Returns the final state of a SHA-256 hash computation.
extern fn sha256_state_handle_digest(state: Sha256StateHandle) -> Box<[u32; 8]> nopanic;

/// Initial hash values for SHA-256 as specified in FIPS 180-4.
const SHA256_INITIAL_STATE: [u32; 8] = [
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
];

/// Computes the SHA-256 hash of an array of 32-bit words.
///
/// # Arguments
///
/// * `input` - An array of `u32` values to hash
/// * `last_input_word` - The final word when input is not word-aligned
/// * `last_input_num_bytes` - Number of bytes in the last input word (must be less than 4)
///
/// # Returns
///
/// * The SHA-256 hash of the `input array` + `last_input_word` as big endian
///
/// # Examples
///
/// ```
/// use core::sha256::compute_sha256_u32_array;
///
/// let hash = compute_sha256_u32_array(array![0x68656c6c], 0x6f, 1);
/// assert!(hash == [0x2cf24dba, 0x5fb0a30e, 0x26e83b2a, 0xc5b9e29e, 0x1b161e5c, 0x1fa7425e,
/// 0x73043362, 0x938b9824]);
/// ```
pub fn compute_sha256_u32_array(
    mut input: Array<u32>, last_input_word: u32, last_input_num_bytes: u32,
) -> [u32; 8] {
    add_sha256_padding(ref input, last_input_word, last_input_num_bytes);

    let mut input = input.span();
    let mut state = sha256_state_handle_init(BoxTrait::new(SHA256_INITIAL_STATE));

    while let Some(chunk) = input.multi_pop_front() {
        state = starknet::syscalls::sha256_process_block_syscall(state, *chunk).unwrap_syscall();
    }

    sha256_state_handle_digest(state).unbox()
}

/// Computes the SHA-256 hash of the input `ByteArray`.
///
/// # Examples
///
/// ```
/// use core::sha256::compute_sha256_byte_array;
///
//! let data = "Hello";
//! let hash = compute_sha256_byte_array(@data);
//! assert!(hash == [0x185f8db3, 0x2271fe25, 0xf561a6fc, 0x938b2e26, 0x4306ec30, 0x4eda5180,
//! 0x7d17648, 0x26381969]);
/// ```
pub fn compute_sha256_byte_array(arr: @ByteArray) -> [u32; 8] {
    let mut span = Span { snapshot: arr.data };

    let mut words_arr = array![];
    let mut last_word = 0;
    let mut last_word_len = 0;
    while let Option::Some(byte_31) = span.pop_front() {
        let u256 { mut low, mut high } = (*byte_31).into();
        let mut shift = BYTES_IN_U128 - 1 - 4 + last_word_len;
        append_u128_to_words_array(ref words_arr, high, ref last_word, ref last_word_len, shift);
        let mut shift = BYTES_IN_U128 - 4 + last_word_len;
        append_u128_to_words_array(ref words_arr, low, ref last_word, ref last_word_len, shift);
    };
    let u256 { mut low, mut high } = (*arr.pending_word).into();
    if *arr.pending_word_len > BYTES_IN_U128 {
        let mut shift = *arr.pending_word_len - BYTES_IN_U128 + last_word_len;
        if shift >= 4 {
            shift = shift - 4;
            append_u128_to_words_array_partial(
                ref words_arr, high, ref last_word, ref last_word_len, shift
            );
        } else {
            last_word = (last_word.into() * one_shift_left_bytes_u128(last_word_len) + high)
                .try_into()
                .unwrap();
            last_word_len = last_word_len + *arr.pending_word_len - BYTES_IN_U128;
        }
        let mut shift = BYTES_IN_U128 - 4 + last_word_len;
        append_u128_to_words_array(ref words_arr, low, ref last_word, ref last_word_len, shift);
    } else {
        let mut shift = *arr.pending_word_len + last_word_len;
        if shift >= 4 {
            shift = shift - 4;
            append_u128_to_words_array_partial(
                ref words_arr, low, ref last_word, ref last_word_len, shift
            );
        } else {
            last_word = (last_word.into() * one_shift_left_bytes_u128(*arr.pending_word_len) + low)
            .try_into()
            .unwrap();
            last_word_len = last_word_len + *arr.pending_word_len;
        }
    }

    return compute_sha256_u32_array(words_arr, last_word, last_word_len);
}

/// Adds padding to the input array according to the SHA-256 specification.
///
/// The padding follows FIPS 180-4:
/// 1. Append a single '1' bit to
/// 2. Append zeros until data length ≡ 448 (mod 512)
/// 3. Append the original message length as a 64-bit big-endian integer
///
/// # Arguments
/// * `arr` - Array to pad (modified in place)
/// * `last_input_word` - Final word for non-word-aligned inputs
/// * `last_input_num_bytes` - Number of valid bytes in last_input_word
fn add_sha256_padding(ref arr: Array<u32>, last_input_word: u32, last_input_num_bytes: u32) {
    let len = arr.len();
    if last_input_num_bytes == 0 {
        arr.append(0x80000000);
    } else {
        let (q, m, pad) = if last_input_num_bytes == 1 {
            (0x100, 0x1000000, 0x800000)
        } else if last_input_num_bytes == 2 {
            (0x10000, 0x10000, 0x8000)
        } else {
            (0x1000000, 0x100, 0x80)
        };
        let (_, r) = crate::integer::u32_safe_divmod(last_input_word, q);
        arr.append(r * m + pad);
    }

    let mut remaining: felt252 = 16 - ((arr.len() + 1) % 16).into();

    append_zeros(ref arr, remaining);

    arr.append(len * 32 + last_input_num_bytes * 8);
}

/// Appends `count` zeros to the array.
fn append_zeros(ref arr: Array<u32>, count: felt252) {
    if count == 0 {
        return;
    }
    arr.append(0);
    if count == 1 {
        return;
    }
    arr.append(0);
    if count == 2 {
        return;
    }
    arr.append(0);
    if count == 3 {
        return;
    }
    arr.append(0);
    if count == 4 {
        return;
    }
    arr.append(0);
    if count == 5 {
        return;
    }
    arr.append(0);
    if count == 6 {
        return;
    }
    arr.append(0);
    if count == 7 {
        return;
    }
    arr.append(0);
    if count == 8 {
        return;
    }
    arr.append(0);
    if count == 9 {
        return;
    }
    arr.append(0);
    if count == 10 {
        return;
    }
    arr.append(0);
    if count == 11 {
        return;
    }
    arr.append(0);
    if count == 12 {
        return;
    }
    arr.append(0);
    if count == 13 {
        return;
    }
    arr.append(0);
    if count == 14 {
        return;
    }
    arr.append(0);
    if count == 15 {
        return;
    }
    arr.append(0);
}

const BYTES_IN_U128: usize = 16;

/// Appends `val` to the words array.
fn append_u128_to_words_array_partial(
    ref words_arr: Array<u32>, val: u128, ref last_word: u32, ref last_word_len: u32, mut shift: u32
) {
    words_arr
        .append(
            (last_word.into() * one_shift_left_bytes_u128(4 - last_word_len)
                + val / one_shift_left_bytes_u128(shift) % one_shift_left_bytes_u128(4))
                .try_into()
                .unwrap()
        );
    if shift < 4 {
        last_word = (val % one_shift_left_bytes_u128(shift)).try_into().unwrap();
        last_word_len = shift;
        return;
    }
    shift = shift - 4;
    words_arr
        .append(
            (val / one_shift_left_bytes_u128(shift) % one_shift_left_bytes_u128(4))
                .try_into()
                .unwrap()
        );
    if shift < 4 {
        last_word = (val % one_shift_left_bytes_u128(shift)).try_into().unwrap();
        last_word_len = shift;
        return;
    }
    shift = shift - 4;
    words_arr
        .append(
            (val / one_shift_left_bytes_u128(shift) % one_shift_left_bytes_u128(4))
                .try_into()
                .unwrap()
        );
    if shift < 4 {
        last_word = (val % one_shift_left_bytes_u128(shift)).try_into().unwrap();
        last_word_len = shift;
        return;
    }
    shift = shift - 4;
    words_arr
        .append(
            (val / one_shift_left_bytes_u128(shift) % one_shift_left_bytes_u128(4))
                .try_into()
                .unwrap()
        );
    last_word = (val % one_shift_left_bytes_u128(shift)).try_into().unwrap();
    last_word_len = shift;
}

/// Appends `val` to the words array.
fn append_u128_to_words_array(
    ref words_arr: Array<u32>, val: u128, ref last_word: u32, ref last_word_len: u32, mut shift: u32
) {
    words_arr
        .append(
            (last_word.into() * one_shift_left_bytes_u128(4 - last_word_len)
                + val / one_shift_left_bytes_u128(shift) % one_shift_left_bytes_u128(4))
                .try_into()
                .unwrap()
        );
    shift = shift - 4;
    words_arr
        .append(
            (val / one_shift_left_bytes_u128(shift) % one_shift_left_bytes_u128(4))
                .try_into()
                .unwrap()
        );
    shift = shift - 4;
    words_arr
        .append(
            (val / one_shift_left_bytes_u128(shift) % one_shift_left_bytes_u128(4))
                .try_into()
                .unwrap()
        );
    if shift != 3 {
        shift = shift - 4;
        words_arr
            .append(
                (val / one_shift_left_bytes_u128(shift) % one_shift_left_bytes_u128(4))
                    .try_into()
                    .unwrap()
            );
    }
    last_word = (val % one_shift_left_bytes_u128(shift)).try_into().unwrap();
    last_word_len = shift;
}
