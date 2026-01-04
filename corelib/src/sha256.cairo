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
#[feature("byte-span")]
use core::byte_array::ToByteSpanTrait;
#[feature("bounded-int-utils")]
use core::internal::bounded_int::{BoundedInt, downcast, upcast};
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

/// Computes the SHA-256 hash of an input provided as 32-bit words, with optional trailing bytes.
///
/// # Note
///
/// For better type safety, consider using `compute_sha256_u32_array_safe` when
/// `last_input_num_bytes` is guaranteed to be in the range 0..=3.
///
/// # Arguments
///
/// * `input` - The main input, expressed as an array of `u32` words.
/// * `last_input_word` - A partial final word containing any remaining bytes when the input is not
/// word-aligned.
/// * `last_input_num_bytes` - The number of valid bytes in `last_input_word`. Must be in the range
/// 0..=3.
///
/// # Panics
///
/// * If `last_input_num_bytes` is greater than 3.
///
/// # Returns
///
/// * The SHA-256 hash of `input` followed by the `last_input_num_bytes` most significant bytes of
/// `last_input_word`, interpreted in big-endian order.
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
    let last_input_num_bytes = downcast(last_input_num_bytes).expect('`last_input_num_bytes` > 3');
    compute_sha256_u32_array_safe(input, last_input_word, last_input_num_bytes)
}

/// A type representing a bounded integer in the range `0..=3`.
pub type u2 = BoundedInt<0, 3>;

/// Computes the SHA-256 hash of an input provided as 32-bit words, with optional trailing bytes.
///
/// # Arguments
///
/// * `input` - The main input, expressed as an array of `u32` words.
/// * `last_input_word` - A partial final word containing any remaining bytes when the input is not
/// word-aligned.
/// * `last_input_num_bytes` - The number of valid bytes in `last_input_word`.
///
/// # Returns
///
/// * The SHA-256 hash of `input` followed by the `last_input_num_bytes` most significant bytes of
/// `last_input_word`, interpreted in big-endian order.
///
/// # Examples
///
/// ```
/// use core::sha256::compute_sha256_u32_array_safe;
///
/// let hash = compute_sha256_u32_array_safe(array![0x68656c6c], 0x6f, 1);
/// assert!(hash == [0x2cf24dba, 0x5fb0a30e, 0x26e83b2a, 0xc5b9e29e, 0x1b161e5c, 0x1fa7425e,
/// 0x73043362, 0x938b9824]);
/// ```
pub fn compute_sha256_u32_array_safe(
    mut input: Array<u32>, last_input_word: u32, last_input_num_bytes: u2,
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
/// let data = "Hello";
/// let hash = compute_sha256_byte_array(@data);
/// assert!(hash == [0x185f8db3, 0x2271fe25, 0xf561a6fc, 0x938b2e26, 0x4306ec30, 0x4eda5180,
/// 0x7d17648, 0x26381969]);
/// ```
pub fn compute_sha256_byte_array(arr: @ByteArray) -> [u32; 8] {
    let mut iter = arr.span().into_iter();
    let mut word_arr = array![];

    let (last_word, last_word_len) = loop {
        let Some(b0) = iter.next() else {
            break (0, 0);
        };
        let Some(b1) = iter.next() else {
            break (upcast(b0), 1);
        };
        let b0_b1 = conversions::shift_append_byte(b0, b1);
        let Some(b2) = iter.next() else {
            break (upcast(b0_b1), 2);
        };
        let b0_b1_b2 = conversions::shift_append_byte(b0_b1, b2);
        let Some(b3) = iter.next() else {
            break (upcast(b0_b1_b2), 3);
        };

        word_arr.append(upcast(conversions::shift_append_byte(b0_b1_b2, b3)));
    };

    compute_sha256_u32_array_safe(word_arr, last_word, last_word_len)
}

/// Adds padding to the input array according to the SHA-256 specification.
///
/// The padding follows FIPS 180-4:
/// 1. Append a single '1' bit to the message.
/// 2. Append zeros until data length ≡ 448 (mod 512).
/// 3. Append the original message length as a 64-bit big-endian integer.
///    This implementation is made for provable execution and assumes the message bit length
///    fits in 32 bits, with the high 32 bits of the 64-bit length field being zero.
///
/// # Arguments
/// * `arr` - Array to pad (modified in place)
/// * `last_input_word` - A partial final word containing any remaining bytes when the input is not
/// word-aligned.
/// * `last_input_num_bytes` - The number of valid bytes in `last_input_word`.
fn add_sha256_padding(ref arr: Array<u32>, last_input_word: u32, last_input_num_bytes: u2) {
    let bitlen = conversions::bitlen(arr.len(), last_input_num_bytes);
    arr.append(conversions::to_last_word(last_input_word, last_input_num_bytes));
    // Now writing the length in bits, at the end of a block.
    // We always need to append a zero to the array, as the length is actually the two last u32
    // words, in which we assume the least significant is enough.
    let zero = 0;
    arr.append(zero);
    // Now we make sure we fill all the remaining space in the block, without the last word.
    let mut remaining = 15 - (arr.len() % 16).into();
    repeatedly_append_value(ref arr, remaining, zero);
    arr.append(bitlen);
}

/// Appends `count` `value`s to the array.
fn repeatedly_append_value(ref arr: Array<u32>, count: felt252, value: u32) {
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

/// Macro to decrement a counter and append a value to an array, or return if the counter is value.
macro dec_and_append_or_return {
    ($remaining: ident, $arr: ident, $value: ident) => {
        if $remaining == 0 {
            return;
        }
        $remaining -= 1;
        $arr.append($value);
    };
}

mod conversions {
    #[feature("bounded-int-utils")]
    use core::internal::bounded_int::{
        self, AddHelper, BoundedInt, DivRemHelper, MulHelper, UnitInt,
    };
    use super::u2;

    impl U8Shift of MulHelper<u8, UnitInt<0x100>> {
        type Result = BoundedInt<0, 0xFF00>;
    }
    impl U8ShiftAddU8 of AddHelper<U8Shift::Result, u8> {
        type Result = BoundedInt<0, 0xFFFF>;
    }
    impl U16Shift of MulHelper<U8ShiftAddU8::Result, UnitInt<0x100>> {
        type Result = BoundedInt<0, 0xFFFF00>;
    }
    impl U16ShiftAddU8 of AddHelper<U16Shift::Result, u8> {
        type Result = BoundedInt<0, 0xFFFFFF>;
    }
    impl U24Shift of MulHelper<U16ShiftAddU8::Result, UnitInt<0x100>> {
        type Result = BoundedInt<0, 0xFFFFFF00>;
    }
    impl U24ShiftAddU8 of AddHelper<U24Shift::Result, u8> {
        type Result = BoundedInt<0, 0xFFFFFFFF>;
    }
    trait ShiftAppendByte<T, MulResult> {
        impl Mul: MulHelper<T, UnitInt<0x100>>;
        impl Add: AddHelper<MulResult, u8>;
    }

    // Blanket implementation for any type with appropriate helpers.
    impl ShiftAppendByteImpl<
        T, impl MH: MulHelper<T, UnitInt<0x100>>, impl AH: AddHelper<MH::Result, u8>,
    > of ShiftAppendByte<T, MH::Result> {}

    /// Shifts a word left by one byte position and appends a new byte.
    pub fn shift_append_byte<
        T,
        MulResult,
        impl SAB: ShiftAppendByte<T, MulResult>,
        +core::metaprogramming::TypeEqual<MulResult, SAB::Mul::Result>,
    >(
        word: T, byte: u8,
    ) -> SAB::Add::Result {
        bounded_int::add::<_, _, SAB::Add>(bounded_int::mul::<_, _, SAB::Mul>(word, 0x100), byte)
    }

    impl DoubleU32 of MulHelper<u32, UnitInt<2>> {
        type Result = BoundedInt<0, 0x1FFFFFFFE>;
    }

    impl DoubleU32Inc of AddHelper<BoundedInt<0, 0x1FFFFFFFE>, UnitInt<1>> {
        type Result = BoundedInt<1, 0x1FFFFFFFF>;
    }

    impl DoubleU32IncRightPadded of MulHelper<BoundedInt<1, 0x1FFFFFFFF>, BoundedInt<0, 0x800000>> {
        type Result = BoundedInt<0, 0xFFFFFFFF800000>;
    }

    impl DoubleU32IncRightPaddedTrim of DivRemHelper<
        BoundedInt<0, 0xFFFFFFFF800000>, UnitInt<0x100000000>,
    > {
        type DivT = BoundedInt<0, 0xFFFFFF>;
        type RemT = BoundedInt<0, 0xFFFFFFFF>;
    }

    /// Returns the last word to append to the input `u32` array given the last word input.
    pub fn to_last_word(word: u32, len: u2) -> u32 {
        let shift: BoundedInt<0, 0x800000> = match len {
            0 => { return 0x80000000; },
            1 => 0x800000,
            2 => 0x8000,
            _ => 0x80,
        };
        let doubled = bounded_int::mul::<_, _, DoubleU32>(word, 2);
        let incremented = bounded_int::add::<_, _, DoubleU32Inc>(doubled, 1);
        let result = bounded_int::mul::<_, _, DoubleU32IncRightPadded>(incremented, shift);
        // For backwards compatibility, we make sure to ignore high bits.
        let (_, r) = bounded_int::div_rem::<_, _, DoubleU32IncRightPaddedTrim>(result, 0x100000000);
        bounded_int::upcast(r)
    }

    impl ArrBitLen of MulHelper<u32, UnitInt<32>> {
        type Result = BoundedInt<0, 0x1FFFFFFFE0>;
    }

    impl WordBitLen of MulHelper<u2, UnitInt<8>> {
        type Result = BoundedInt<0, 0x18>;
    }

    impl FullBitLen of AddHelper<BoundedInt<0, 0x1FFFFFFFE0>, BoundedInt<0, 0x18>> {
        type Result = BoundedInt<0, { 0x1FFFFFFFE0 + 0x18 }>;
    }

    /// Returns the bit length of the input message, given the length of the representation u32
    /// array and last word bytes.
    pub fn bitlen(arr_len: u32, last_word_bytes: u2) -> u32 {
        let arr_bits = bounded_int::mul::<_, _, ArrBitLen>(arr_len, 32);
        let last_word_bits = bounded_int::mul::<_, _, WordBitLen>(last_word_bytes, 8);
        bounded_int::downcast(bounded_int::add::<_, _, FullBitLen>(arr_bits, last_word_bits))
            .unwrap()
    }
}
