//! Implementation of the SHA-384 cryptographic hash function.
//!
//! This module provides functions to compute SHA-384 hashes of data.
//! The input data can be an array of 64-bit words, or a `ByteArray`.
//!
//! # Examples
//!
//! ```
//! use core::sha384::compute_sha384_byte_array;
//!
//! let data = "Hello world";
//! let hash = compute_sha384_byte_array(@data);
//! assert!(
//!     hash == [
//!         0x9203b0c4439fd1e6, 0xae5878866337b7c5, 0x32acd6d9260150c8,
//!         0x0318e8ab8c27ce33, 0x0189f8df94fb890d, 0xf1d298ff360627e1,
//!     ],
//! );
//! ```

pub use crate::sha2_64_core::u3;
use crate::sha2_64_core::{compute_sha2_64_byte_array, compute_sha2_64_u64_array};

/// Initial hash values for SHA-384 as specified in FIPS 180-4.
const SHA384_INITIAL_STATE: [u64; 8] = [
    0xcbbb9d5dc1059ed8, 0x629a292a367cd507, 0x9159015a3070dd17, 0x152fecd8f70e5939,
    0x67332667ffc00b31, 0x8eb44a8768581511, 0xdb0c2e0d64f98fa7, 0x47b5481dbefa4fa4,
];

/// Computes the SHA-384 hash of an input provided as 64-bit words, with optional trailing bytes.
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
/// * The SHA-384 hash of `input` followed by the `last_input_num_bytes` most significant bytes of
/// `last_input_word`, interpreted in big-endian order.
///
/// # Examples
///
/// ```
/// use core::sha384::compute_sha384_u64_array;
///
/// // SHA-384("Hello world")
/// let hash = compute_sha384_u64_array(array![0x48656c6c6f20776f], 0x726c64, 3);
/// assert!(
///     hash == [
///         0x9203b0c4439fd1e6, 0xae5878866337b7c5, 0x32acd6d9260150c8,
///         0x0318e8ab8c27ce33, 0x0189f8df94fb890d, 0xf1d298ff360627e1,
///     ],
/// );
/// ```
pub fn compute_sha384_u64_array(
    mut input: Array<u64>, last_input_word: u64, last_input_num_bytes: u3,
) -> [u64; 6] {
    truncate_hash(
        compute_sha2_64_u64_array(
            input, last_input_word, last_input_num_bytes, BoxTrait::new(SHA384_INITIAL_STATE),
        ),
    )
}

/// Computes the SHA-384 hash of the input `ByteArray`.
///
/// # Examples
///
/// ```
/// use core::sha384::compute_sha384_byte_array;
///
/// let data = "Hello world";
/// let hash = compute_sha384_byte_array(@data);
/// assert!(
///     hash == [
///         0x9203b0c4439fd1e6, 0xae5878866337b7c5, 0x32acd6d9260150c8,
///         0x0318e8ab8c27ce33, 0x0189f8df94fb890d, 0xf1d298ff360627e1,
///     ],
/// );
/// ```
pub fn compute_sha384_byte_array(arr: @ByteArray) -> [u64; 6] {
    truncate_hash(compute_sha2_64_byte_array(arr, BoxTrait::new(SHA384_INITIAL_STATE)))
}

/// Truncates a SHA-384 hash to 48 bytes by dropping the final 16 bytes.
fn truncate_hash(hash: Box<[u64; 8]>) -> [u64; 6] {
    let [w0, w1, w2, w3, w4, w5, _w6, _w7] = hash.unbox();
    [w0, w1, w2, w3, w4, w5]
}
