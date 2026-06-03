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
//! let data = "Hello world";
//! let hash = compute_sha512_byte_array(@data);
//! assert!(
//!     hash == [
//!         0xb7f783baed8297f0, 0xdb917462184ff4f0, 0x8e69c2d5e5f79a94, 0x2600f9725f58ce1f,
//!         0x29c18139bf80b06c, 0x0fff2bdd34738452, 0xecf40c488c22a7e3, 0xd80cdf6f9c1c0d47,
//!     ],
//! );
//! ```

pub use crate::sha2_64_core::u3;
use crate::sha2_64_core::{compute_sha2_64_byte_array, compute_sha2_64_u64_array};

/// Initial hash values for SHA-512 as specified in FIPS 180-4.
const SHA512_INITIAL_STATE: [u64; 8] = [
    0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
    0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179,
];

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
/// * The SHA-512 hash of `input` followed by the `last_input_num_bytes` least significant bytes of
/// `last_input_word`, interpreted in big-endian order.
///
/// # Examples
///
/// ```
/// use core::sha512::compute_sha512_u64_array;
///
/// // SHA-512("Hello world")
/// let hash = compute_sha512_u64_array(array![0x48656c6c6f20776f], 0x726c64, 3);
/// assert!(
///     hash == [
///         0xb7f783baed8297f0, 0xdb917462184ff4f0, 0x8e69c2d5e5f79a94, 0x2600f9725f58ce1f,
///         0x29c18139bf80b06c, 0x0fff2bdd34738452, 0xecf40c488c22a7e3, 0xd80cdf6f9c1c0d47,
///     ],
/// );
/// ```
pub fn compute_sha512_u64_array(
    mut input: Array<u64>, last_input_word: u64, last_input_num_bytes: u3,
) -> [u64; 8] {
    compute_sha2_64_u64_array(
        input, last_input_word, last_input_num_bytes, BoxTrait::new(SHA512_INITIAL_STATE),
    )
        .unbox()
}

/// Computes the SHA-512 hash of the input `ByteArray`.
///
/// # Examples
///
/// ```
/// use core::sha512::compute_sha512_byte_array;
///
/// let data = "Hello world";
/// let hash = compute_sha512_byte_array(@data);
/// assert!(
///     hash == [
///         0xb7f783baed8297f0, 0xdb917462184ff4f0, 0x8e69c2d5e5f79a94, 0x2600f9725f58ce1f,
///         0x29c18139bf80b06c, 0x0fff2bdd34738452, 0xecf40c488c22a7e3, 0xd80cdf6f9c1c0d47,
///     ],
/// );
/// ```
pub fn compute_sha512_byte_array(arr: @ByteArray) -> [u64; 8] {
    compute_sha2_64_byte_array(arr, BoxTrait::new(SHA512_INITIAL_STATE)).unbox()
}
