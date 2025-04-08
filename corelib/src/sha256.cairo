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
    let mut word_arr = array![];
    let len = arr.len();
    let rem = len % 4;
    let mut index = 0;
    let rounded_len = len - rem;
    while index != rounded_len {
        let word = arr.at(index + 3).unwrap().into()
            + arr.at(index + 2).unwrap().into() * 0x100
            + arr.at(index + 1).unwrap().into() * 0x10000
            + arr.at(index).unwrap().into() * 0x1000000;
        word_arr.append(word);
        index = index + 4;
    }

    let last = match rem {
        0 => 0,
        1 => arr.at(len - 1).unwrap().into(),
        2 => arr.at(len - 1).unwrap().into() + arr.at(len - 2).unwrap().into() * 0x100,
        _ => arr.at(len - 1).unwrap().into()
            + arr.at(len - 2).unwrap().into() * 0x100
            + arr.at(len - 3).unwrap().into() * 0x10000,
    };

    compute_sha256_u32_array(word_arr, last, rem.into())
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
