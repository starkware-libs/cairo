//! Implementation of the SHA-512 cryptographic hash function.
//!
//! This module provides functions to compute SHA-512 hashes of data.
//! The input data can be an array of 64-bit words, or a `ByteArray`.
//!
//! # Examples
//!
//! ```
//! use core::sha512::compute_sha512_byte_array;
//!
//! let data = "Hello";
//! let hash = compute_sha512_byte_array(@data);
//! assert!(
//!     hash == [
//!         0x3615f80c9d293ed7, 0x402687f94b22d58e, 0x529b8cc7916f8fac, 0x7fddf7fbd5af4cf7,
//!         0x77d3d795a7a00a16, 0xbf7e7f3fb9561ee9, 0xbaae480da9fe7a18, 0x769e71886b03f315,
//!     ],
//! );
//! ```
#[feature("byte-span")]
use core::byte_array::ToByteSpanTrait;
#[feature("bounded-int-utils")]
use core::internal::bounded_int::{BoundedInt, downcast};
use starknet::SyscallResultTrait;

/// A handle to the state of a SHA-512 hash.
pub(crate) extern type Sha512StateHandle;

impl Sha512StateHandleCopy of Copy<Sha512StateHandle>;
impl Sha512StateHandleDrop of Drop<Sha512StateHandle>;

/// Initializes a new SHA-512 state handle with the given initial state.
extern fn sha512_state_handle_init(state: Box<[u64; 8]>) -> Sha512StateHandle nopanic;

/// Returns the final state of a SHA-512 hash computation.
extern fn sha512_state_handle_digest(state: Sha512StateHandle) -> Box<[u64; 8]> nopanic;

/// Initial hash values for SHA-512 as specified in FIPS 180-4.
const SHA512_INITIAL_STATE: [u64; 8] = [
    0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
    0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179,
];

/// A type representing a bounded integer in the range `0..=7`.
pub type u3 = BoundedInt<0, 7>;

/// Computes the SHA-512 hash of an input provided as 64-bit words, with optional trailing bytes.
///
/// # Note
///
/// For better type safety, consider using `compute_sha512_u64_array_safe` when
/// `last_input_num_bytes` is guaranteed to be in the range 0..=7.
///
/// # Arguments
///
/// * `input` - The main input, expressed as an array of `u64` words.
/// * `last_input_word` - A partial final word containing any remaining bytes when the input is not
/// word-aligned.
/// * `last_input_num_bytes` - The number of valid bytes in `last_input_word`. Must be in the range
/// 0..=7.
///
/// # Panics
///
/// * If `last_input_num_bytes` is greater than 7.
///
/// # Returns
///
/// * The SHA-512 hash of `input` followed by the `last_input_num_bytes` most significant bytes of
/// `last_input_word`, interpreted in big-endian order.
pub fn compute_sha512_u64_array(
    mut input: Array<u64>, last_input_word: u64, last_input_num_bytes: u64,
) -> [u64; 8] {
    let last_input_num_bytes = downcast(last_input_num_bytes).expect('`last_input_num_bytes` > 7');
    compute_sha512_u64_array_safe(input, last_input_word, last_input_num_bytes)
}

/// Computes the SHA-512 hash of an input provided as 64-bit words, with optional trailing bytes.
///
/// # Arguments
///
/// * `input` - The main input, expressed as an array of `u64` words.
/// * `last_input_word` - A partial final word containing any remaining bytes when the input is not
/// word-aligned.
/// * `last_input_num_bytes` - The number of valid bytes in `last_input_word`.
///
/// # Returns
///
/// * The SHA-512 hash of `input` followed by the `last_input_num_bytes` most significant bytes of
/// `last_input_word`, interpreted in big-endian order.
pub fn compute_sha512_u64_array_safe(
    mut input: Array<u64>, last_input_word: u64, last_input_num_bytes: u3,
) -> [u64; 8] {
    add_sha512_padding(ref input, last_input_word, last_input_num_bytes);

    let mut input = input.span();
    let mut state = sha512_state_handle_init(BoxTrait::new(SHA512_INITIAL_STATE));

    while let Some(chunk) = input.multi_pop_front() {
        state = starknet::syscalls::sha512_process_block_syscall(state, *chunk).unwrap_syscall();
    }

    sha512_state_handle_digest(state).unbox()
}

/// Computes the SHA-512 hash of the input `ByteArray`.
///
/// # Examples
///
/// ```
/// use core::sha512::compute_sha512_byte_array;
///
/// let data = "abc";
/// let hash = compute_sha512_byte_array(@data);
/// assert!(
///     hash == [
///         0xddaf35a193617aba, 0xcc417349ae204131, 0x12e6fa4e89a97ea2, 0x0a9eeee64b55d39a,
///         0x2192992a274fc1a8, 0x36ba3c23a3feebbd, 0x454d4423643ce80e, 0x2a9ac94fa54ca49f,
///     ],
/// );
/// ```
pub fn compute_sha512_byte_array(arr: @ByteArray) -> [u64; 8] {
    let mut iter = arr.span().into_iter();
    let mut word_arr: Array<u64> = array![];

    let (last_word, last_word_len): (u64, u3) = loop {
        let Some(b0) = iter.next() else {
            break (0, 0);
        };
        let Some(b1) = iter.next() else {
            break (pack_u64(b0, 0, 0, 0, 0, 0, 0, 0), 1);
        };
        let Some(b2) = iter.next() else {
            break (pack_u64(b0, b1, 0, 0, 0, 0, 0, 0), 2);
        };
        let Some(b3) = iter.next() else {
            break (pack_u64(b0, b1, b2, 0, 0, 0, 0, 0), 3);
        };
        let Some(b4) = iter.next() else {
            break (pack_u64(b0, b1, b2, b3, 0, 0, 0, 0), 4);
        };
        let Some(b5) = iter.next() else {
            break (pack_u64(b0, b1, b2, b3, b4, 0, 0, 0), 5);
        };
        let Some(b6) = iter.next() else {
            break (pack_u64(b0, b1, b2, b3, b4, b5, 0, 0), 6);
        };
        let Some(b7) = iter.next() else {
            break (pack_u64(b0, b1, b2, b3, b4, b5, b6, 0), 7);
        };
        word_arr.append(pack_u64(b0, b1, b2, b3, b4, b5, b6, b7));
    };

    compute_sha512_u64_array_safe(word_arr, last_word, last_word_len)
}

/// Packs 8 bytes into a single big-endian `u64` word.
fn pack_u64(b0: u8, b1: u8, b2: u8, b3: u8, b4: u8, b5: u8, b6: u8, b7: u8) -> u64 {
    b0.into() * 0x100000000000000_u64
        + b1.into() * 0x1000000000000_u64
        + b2.into() * 0x10000000000_u64
        + b3.into() * 0x100000000_u64
        + b4.into() * 0x1000000_u64
        + b5.into() * 0x10000_u64
        + b6.into() * 0x100_u64
        + b7.into()
}

/// Adds padding to the input array according to the SHA-512 specification (FIPS 180-4).
///
/// 1. Append a single '1' bit.
/// 2. Append zeros until data length ≡ 896 (mod 1024).
/// 3. Append the original message length as a 128-bit big-endian integer.
///    This implementation assumes the message bit length fits in 64 bits (high 64 bits are zero).
fn add_sha512_padding(ref arr: Array<u64>, last_input_word: u64, last_input_num_bytes: u3) {
    let bitlen = message_bit_len(arr.len(), last_input_num_bytes);
    arr.append(to_last_word(last_input_word, last_input_num_bytes));
    // High 64 bits of the 128-bit length field (zero for any practical message size).
    let zero: u64 = 0;
    arr.append(zero);
    // Fill remaining words in the block, leaving one slot for the low length word.
    let mut remaining: felt252 = 15 - (arr.len() % 16).into();
    repeatedly_append_value(ref arr, remaining, zero);
    arr.append(bitlen);
}

/// Returns the last word to append: keeps the top `len` bytes of `word` and inserts a 0x80 marker
/// immediately after them.
fn to_last_word(word: u64, len: u3) -> u64 {
    match len {
        0 => 0x8000000000000000_u64,
        1 => (word & 0xFF00000000000000_u64) | 0x0080000000000000_u64,
        2 => (word & 0xFFFF000000000000_u64) | 0x0000800000000000_u64,
        3 => (word & 0xFFFFFF0000000000_u64) | 0x0000008000000000_u64,
        4 => (word & 0xFFFFFFFF00000000_u64) | 0x0000000080000000_u64,
        5 => (word & 0xFFFFFFFFFF000000_u64) | 0x0000000000800000_u64,
        6 => (word & 0xFFFFFFFFFFFF0000_u64) | 0x0000000000008000_u64,
        _ => (word & 0xFFFFFFFFFFFFFF00_u64) | 0x0000000000000080_u64,
    }
}

/// Returns the message bit length given the number of full 64-bit words and trailing bytes.
fn message_bit_len(arr_len: u32, last_word_bytes: u3) -> u64 {
    let last_bits: u64 = match last_word_bytes {
        0 => 0,
        1 => 8,
        2 => 16,
        3 => 24,
        4 => 32,
        5 => 40,
        6 => 48,
        _ => 56,
    };
    arr_len.into() * 64_u64 + last_bits
}

/// Appends `count` copies of `value` to the array.
fn repeatedly_append_value(ref arr: Array<u64>, count: felt252, value: u64) {
    let mut remaining = count;
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 0`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 1`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 2`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 3`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 4`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 5`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 6`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 7`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 8`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 9`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 10`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 11`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 12`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 13`.
    dec_and_append_or_return!(remaining, arr, value); // returns if `count == 14`.
}

macro dec_and_append_or_return {
    ($remaining: ident, $arr: ident, $value: ident) => {
        if $remaining == 0 {
            return;
        }
        $remaining -= 1;
        $arr.append($value);
    };
}
