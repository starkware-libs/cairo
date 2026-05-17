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
//!         0x3615f80c9d293ed7, 0xceae0ae55b4f1d3e, 0x09ac15d8ff06cb65, 0x573a42e8f0da8c21,
//!         0x7266baf0f5d1e7d1, 0x66e56de0aba98099, 0xb3e25ea1ceee7e79, 0x8093c3e1a406ccba,
//!     ],
//! );
//! ```
#[feature("byte-span")]
use core::byte_array::ToByteSpanTrait;
#[feature("bounded-int-utils")]
use core::internal::bounded_int::{BoundedInt, downcast, upcast};
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
///
/// # Examples
///
/// ```
/// use core::sha512::compute_sha512_u64_array;
///
/// let hash = compute_sha512_u64_array(array![], 0x68656c6c6f, 5);
/// ```
pub fn compute_sha512_u64_array(
    mut input: Array<u64>, last_input_word: u64, last_input_num_bytes: u32,
) -> [u64; 8] {
    let last_input_num_bytes = downcast(last_input_num_bytes).expect('`last_input_num_bytes` > 7');
    compute_sha512_u64_array_safe(input, last_input_word, last_input_num_bytes)
}

/// A type representing a bounded integer in the range `0..=7`.
pub type u3 = BoundedInt<0, 7>;

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
///
/// # Examples
///
/// ```
/// use core::sha512::compute_sha512_u64_array_safe;
///
/// let hash = compute_sha512_u64_array_safe(array![], 0x68656c6c6f, 5);
/// ```
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
/// let data = "Hello";
/// let hash = compute_sha512_byte_array(@data);
/// assert!(
///     hash == [
///         0x3615f80c9d293ed7, 0xceae0ae55b4f1d3e, 0x09ac15d8ff06cb65, 0x573a42e8f0da8c21,
///         0x7266baf0f5d1e7d1, 0x66e56de0aba98099, 0xb3e25ea1ceee7e79, 0x8093c3e1a406ccba,
///     ],
/// );
/// ```
pub fn compute_sha512_byte_array(arr: @ByteArray) -> [u64; 8] {
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
        let b0_b1_b2_b3 = conversions::shift_append_byte(b0_b1_b2, b3);
        let Some(b4) = iter.next() else {
            break (upcast(b0_b1_b2_b3), 4);
        };
        let b0_b1_b2_b3_b4 = conversions::shift_append_byte(b0_b1_b2_b3, b4);
        let Some(b5) = iter.next() else {
            break (upcast(b0_b1_b2_b3_b4), 5);
        };
        let b0_b1_b2_b3_b4_b5 = conversions::shift_append_byte(b0_b1_b2_b3_b4, b5);
        let Some(b6) = iter.next() else {
            break (upcast(b0_b1_b2_b3_b4_b5), 6);
        };
        let b0_b1_b2_b3_b4_b5_b6 = conversions::shift_append_byte(b0_b1_b2_b3_b4_b5, b6);
        let Some(b7) = iter.next() else {
            break (upcast(b0_b1_b2_b3_b4_b5_b6), 7);
        };

        word_arr.append(upcast(conversions::shift_append_byte(b0_b1_b2_b3_b4_b5_b6, b7)));
    };

    compute_sha512_u64_array_safe(word_arr, last_word, last_word_len)
}

/// Adds padding to the input array according to the SHA-512 specification.
///
/// The padding follows FIPS 180-4:
/// 1. Append a single '1' bit to the message.
/// 2. Append zeros until data length ≡ 896 (mod 1024).
/// 3. Append the original message length as a 128-bit big-endian integer.
///    This implementation is made for provable execution and assumes the message bit length
///    fits in 64 bits, with the high 64 bits of the 128-bit length field being zero.
///
/// # Arguments
/// * `arr` - Array to pad (modified in place)
/// * `last_input_word` - A partial final word containing any remaining bytes when the input is not
/// word-aligned.
/// * `last_input_num_bytes` - The number of valid bytes in `last_input_word`.
fn add_sha512_padding(ref arr: Array<u64>, last_input_word: u64, last_input_num_bytes: u3) {
    let bitlen = conversions::bitlen(arr.len(), last_input_num_bytes);
    arr.append(conversions::to_last_word(last_input_word, last_input_num_bytes));
    // Now writing the length in bits, at the end of a block.
    // We always need to append a zero to the array, as the length is actually the two last u64
    // words, in which we assume the least significant is enough.
    let zero = 0;
    arr.append(zero);
    // Now we make sure we fill all the remaining space in the block, without the last word.
    let mut remaining = 15 - (arr.len() % 16).into();
    repeatedly_append_value(ref arr, remaining, zero);
    arr.append(bitlen);
}

/// Appends `count` `value`s to the array.
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
    use super::u3;

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
    impl U32Shift of MulHelper<U24ShiftAddU8::Result, UnitInt<0x100>> {
        type Result = BoundedInt<0, 0xFFFFFFFF00>;
    }
    impl U32ShiftAddU8 of AddHelper<U32Shift::Result, u8> {
        type Result = BoundedInt<0, 0xFFFFFFFFFF>;
    }
    impl U40Shift of MulHelper<U32ShiftAddU8::Result, UnitInt<0x100>> {
        type Result = BoundedInt<0, 0xFFFFFFFFFF00>;
    }
    impl U40ShiftAddU8 of AddHelper<U40Shift::Result, u8> {
        type Result = BoundedInt<0, 0xFFFFFFFFFFFF>;
    }
    impl U48Shift of MulHelper<U40ShiftAddU8::Result, UnitInt<0x100>> {
        type Result = BoundedInt<0, 0xFFFFFFFFFFFF00>;
    }
    impl U48ShiftAddU8 of AddHelper<U48Shift::Result, u8> {
        type Result = BoundedInt<0, 0xFFFFFFFFFFFFFF>;
    }
    impl U56Shift of MulHelper<U48ShiftAddU8::Result, UnitInt<0x100>> {
        type Result = BoundedInt<0, 0xFFFFFFFFFFFFFF00>;
    }
    impl U56ShiftAddU8 of AddHelper<U56Shift::Result, u8> {
        type Result = BoundedInt<0, 0xFFFFFFFFFFFFFFFF>;
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

    impl DoubleU64 of MulHelper<u64, UnitInt<2>> {
        type Result = BoundedInt<0, 0x1FFFFFFFFFFFFFFFE>;
    }

    impl DoubleU64Inc of AddHelper<BoundedInt<0, 0x1FFFFFFFFFFFFFFFE>, UnitInt<1>> {
        type Result = BoundedInt<1, 0x1FFFFFFFFFFFFFFFF>;
    }

    impl DoubleU64IncRightPadded of MulHelper<
        BoundedInt<1, 0x1FFFFFFFFFFFFFFFF>, BoundedInt<0, 0x80000000000000>,
    > {
        type Result = BoundedInt<0, 0xFFFFFFFFFFFFFFFF80000000000000>;
    }

    impl DoubleU64IncRightPaddedTrim of DivRemHelper<
        BoundedInt<0, 0xFFFFFFFFFFFFFFFF80000000000000>, UnitInt<0x10000000000000000>,
    > {
        type DivT = BoundedInt<0, 0xFFFFFFFFFFFFFF>;
        type RemT = BoundedInt<0, 0xFFFFFFFFFFFFFFFF>;
    }

    /// Returns the last word to append to the input `u64` array given the last word input.
    pub fn to_last_word(word: u64, len: u3) -> u64 {
        let shift: BoundedInt<0, 0x80000000000000> = match len {
            0 => { return 0x8000000000000000; },
            1 => 0x80000000000000,
            2 => 0x800000000000,
            3 => 0x8000000000,
            4 => 0x80000000,
            5 => 0x800000,
            6 => 0x8000,
            _ => 0x80,
        };
        let doubled = bounded_int::mul::<_, _, DoubleU64>(word, 2);
        let incremented = bounded_int::add::<_, _, DoubleU64Inc>(doubled, 1);
        let result = bounded_int::mul::<_, _, DoubleU64IncRightPadded>(incremented, shift);
        // For backwards compatibility, we make sure to ignore high bits.
        let (_, r) = bounded_int::div_rem::<
            _, _, DoubleU64IncRightPaddedTrim,
        >(result, 0x10000000000000000);
        bounded_int::upcast(r)
    }

    impl ArrBitLen of MulHelper<u32, UnitInt<64>> {
        type Result = BoundedInt<0, 0x3FFFFFFFC0>;
    }

    impl WordBitLen of MulHelper<u3, UnitInt<8>> {
        type Result = BoundedInt<0, 0x38>;
    }

    impl FullBitLen of AddHelper<BoundedInt<0, 0x3FFFFFFFC0>, BoundedInt<0, 0x38>> {
        type Result = BoundedInt<0, { 0x3FFFFFFFC0 + 0x38 }>;
    }

    /// Returns the bit length of the input message, given the length of the representation u64
    /// array and last word bytes.
    pub fn bitlen(arr_len: u32, last_word_bytes: u3) -> u64 {
        let arr_bits = bounded_int::mul::<_, _, ArrBitLen>(arr_len, 64);
        let last_word_bits = bounded_int::mul::<_, _, WordBitLen>(last_word_bytes, 8);
        bounded_int::upcast(bounded_int::add::<_, _, FullBitLen>(arr_bits, last_word_bits))
    }
}
