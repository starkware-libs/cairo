//! `ByteArray` is designed to handle large sequences of bytes with operations like appending,
//! concatenation, and accessing individual bytes. It uses a structure that combines an `Array` of
//! `bytes31` for full words and a `felt252` for handling partial words, optimizing for both space
//! and performance.
//!
//! # Examples
//!
//! There are multiple ways to create a new `ByteArray`:
//! - From a string literal:
//!
//! ```
//! let s = "Hello";
//! ```
//!
//! - Using the `format!` macro:
//!
//! ```
//! let max_tps:u16 = 850;
//! let s = format!("Starknet's max TPS is: {}", max_tps);
//! ```
//!
//! You can append bytes to an existing `ByteArray` with [`ByteArrayTrait::append_byte`]:
//!
//! ```
//! let mut ba: ByteArray = "";
//! ba.append_byte(0x41); // Appending a single byte 'A'
//! ```
//!
//! You can create a new `ByteArray` from an existing one by concatenating with
//! `+`:
//!
//! ```
//! let s = "Hello";
//! let message = s + " world!";
//! ```
//!
//! Indexing operations are available on the `ByteArray` type as well:
//!
//! ```
//! let mut ba: ByteArray = "ABC";
//! let first_byte = ba[0]
//! assert!(first_byte == 0x41);
//! ```

use crate::array::{ArrayTrait, SpanTrait};
#[allow(unused_imports)]
use crate::bytes_31::{
    BYTES_IN_BYTES31, Bytes31Trait, POW_2_128, POW_2_8, U128IntoBytes31, U8IntoBytes31,
    one_shift_left_bytes_felt252, one_shift_left_bytes_u128, split_u128, u8_at_u256,
};
use crate::clone::Clone;
use crate::cmp::min;
#[allow(unused_imports)]
use crate::integer::{U32TryIntoNonZero, u128_safe_divmod};
#[allow(unused_imports)]
use crate::serde::Serde;
use crate::traits::{Into, TryInto};
#[allow(unused_imports)]
use crate::zeroable::NonZeroIntoImpl;

/// A magic constant for identifying serialization of `ByteArray` variables. An array of `felt252`
/// with this magic value as one of the `felt252` indicates that you should expect right after it a
/// serialized `ByteArray`. This is currently used mainly for prints and panics.
pub const BYTE_ARRAY_MAGIC: felt252 =
    0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3;
const BYTES_IN_U128: usize = 16;
const BYTES_IN_BYTES31_MINUS_ONE: usize = BYTES_IN_BYTES31 - 1;

// TODO(yuval): don't allow creation of invalid ByteArray?
/// Byte array type.
#[derive(Drop, Clone, PartialEq, Serde, Default)]
pub struct ByteArray {
    /// An array of full "words" of 31 bytes each.
    /// The first byte of each word in the byte array is the most significant byte in the word.
    pub(crate) data: Array<bytes31>,
    /// A `felt252` that actually represents a `bytes31`, with less than 31 bytes.
    /// It is represented as a `felt252` to improve performance of building the byte array.
    /// The first byte is the most significant byte among the `pending_word_len` bytes in the word.
    pub(crate) pending_word: felt252,
    /// The number of bytes in `pending_word`.
    /// Its value should be in the range [0, 30].
    pub(crate) pending_word_len: usize,
}

pub(crate) impl ByteArrayStringLiteral of crate::string::StringLiteral<ByteArray>;

/// Functions associated with the `ByteArray` type.
#[generate_trait]
pub impl ByteArrayImpl of ByteArrayTrait {
    // TODO(yuval): add a `new` function for initialization.

    /// Appends a single word of `len` bytes to the end of the `ByteArray`.
    ///
    /// This function assumes that:
    /// 1. `word` could be validly converted to a `bytes31` which has no more than `len` bytes
    ///    of data.
    /// 2. len <= BYTES_IN_BYTES31.
    ///
    /// If these assumptions are not met, it can corrupt the `ByteArray`. Thus, this should be a
    /// private function. We could add masking/assertions but it would be more expensive.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut ba = "";
    /// ba.append_word('word', 4);
    /// assert!(ba == "word");
    /// ```
    fn append_word(ref self: ByteArray, word: felt252, len: usize) {
        if len == 0 {
            return;
        }
        let total_pending_bytes = self.pending_word_len + len;

        if total_pending_bytes < BYTES_IN_BYTES31 {
            self.append_word_fits_into_pending(word, len);
            return;
        }

        if total_pending_bytes == BYTES_IN_BYTES31 {
            self
                .data
                .append(
                    (word + self.pending_word * one_shift_left_bytes_felt252(len))
                        .try_into()
                        .unwrap(),
                );
            self.pending_word = 0;
            self.pending_word_len = 0;
            return;
        }

        // The split index is the number of bytes left for the next word (new pending_word of the
        // modified ByteArray).
        let split_index = total_pending_bytes - BYTES_IN_BYTES31;
        if split_index == BYTES_IN_U128 {
            self.append_split_index_16(word);
        } else if split_index < BYTES_IN_U128 {
            self.append_split_index_lt_16(word, split_index);
        } else { // split_index > BYTES_IN_U128
            self.append_split_index_gt_16(word, split_index);
        }
        self.pending_word_len = split_index;
    }

    /// Appends a `ByteArray` to the end of another `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut ba: ByteArray = "1";
    /// ba.append(@"2");
    /// assert!(ba == "12");
    /// ```
    fn append(ref self: ByteArray, other: @ByteArray) {
        let mut other_data = other.data.span();

        if self.pending_word_len == 0 {
            self.data.append_span(other_data);
            self.pending_word = *other.pending_word;
            self.pending_word_len = *other.pending_word_len;
            return;
        }

        // self.pending_word_len is in [1, 30]. This is the split index for all the full words of
        // `other`, as for each word, this is the number of bytes left for the next word.
        if self.pending_word_len == BYTES_IN_U128 {
            loop {
                match other_data.pop_front() {
                    Some(current_word) => { self.append_split_index_16((*current_word).into()); },
                    None => { break; },
                }
            }
        } else if self.pending_word_len < BYTES_IN_U128 {
            loop {
                match other_data.pop_front() {
                    Some(current_word) => {
                        self
                            .append_split_index_lt_16(
                                (*current_word).into(), self.pending_word_len,
                            );
                    },
                    None => { break; },
                }
            }
        } else {
            // self.pending_word_len > BYTES_IN_U128
            loop {
                match other_data.pop_front() {
                    Some(current_word) => {
                        self
                            .append_split_index_gt_16(
                                (*current_word).into(), self.pending_word_len,
                            );
                    },
                    None => { break; },
                }
            }
        }

        // Add the pending word of `other`.
        self.append_word(*other.pending_word, *other.pending_word_len);
    }

    /// Concatenates two `ByteArray` and returns the result.
    ///
    /// The content of `left` is cloned in a new memory segment.
    /// # Examples
    ///
    /// ```
    /// let ba = "1";
    /// let other_ba = "2";
    /// let result = ByteArrayTrait::concat(@ba, @other_ba);
    /// assert!(result == "12");
    /// ```
    fn concat(left: @ByteArray, right: @ByteArray) -> ByteArray {
        let mut result = left.clone();
        result.append(right);
        result
    }

    /// Appends a single byte to the end of the `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut ba = "";
    /// ba.append_byte(0);
    /// assert!(ba == "0");
    /// ```
    fn append_byte(ref self: ByteArray, byte: u8) {
        if self.pending_word_len == 0 {
            self.pending_word = byte.into();
            self.pending_word_len = 1;
            return;
        }

        let new_pending = self.pending_word * POW_2_8.into() + byte.into();

        if self.pending_word_len != BYTES_IN_BYTES31_MINUS_ONE {
            self.pending_word = new_pending;
            self.pending_word_len += 1;
            return;
        }

        // self.pending_word_len == 30
        self.data.append(new_pending.try_into().unwrap());
        self.pending_word = 0;
        self.pending_word_len = 0;
    }

    /// Returns the length of the `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// let ba: ByteArray = "byte array";
    /// let len = ba.len();
    /// assert!(len == 10);
    /// ```
    #[must_use]
    fn len(self: @ByteArray) -> usize {
        self.data.len() * BYTES_IN_BYTES31.into() + (*self.pending_word_len).into()
    }

    /// Returns an option of the byte at the given index of `self`
    /// or `None` if the index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// let ba: ByteArray = "byte array";
    /// let byte = ba.at(0).unwrap();
    /// assert!(byte == 98);
    /// ```
    fn at(self: @ByteArray, index: usize) -> Option<u8> {
        let (word_index, index_in_word) = DivRem::div_rem(index, 31);
        if word_index == self.data.len() {
            // Index is in pending word.
            if index_in_word >= *self.pending_word_len {
                return None;
            }
            // index_in_word is from MSB, we need index from LSB.
            return Some(
                u8_at_u256((*self.pending_word).into(), *self.pending_word_len - 1 - index_in_word),
            );
        }

        let data_word: bytes31 = *self.data.get(word_index)?.deref();
        // index_in_word is from MSB, we need index from LSB.
        Some(data_word.at(BYTES_IN_BYTES31 - 1 - index_in_word))
    }

    /// Returns a `ByteArray` with the reverse order of `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// let ba: ByteArray = "123";
    /// let rev_ba = ba.rev();
    /// assert!(rev_ba == "321");
    /// ```
    fn rev(self: @ByteArray) -> ByteArray {
        let mut result = Default::default();

        result.append_word_rev(*self.pending_word, *self.pending_word_len);

        let mut data = self.data.span();
        loop {
            match data.pop_back() {
                Some(current_word) => {
                    result.append_word_rev((*current_word).into(), BYTES_IN_BYTES31);
                },
                None => { break; },
            }
        }
        result
    }

    /// Appends the reverse of the given word to the end of `self`.
    ///
    /// This function assumes that:
    /// 1. len < 31
    /// 2. word is validly convertible to bytes31 of length `len`.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut ba: ByteArray = "";
    /// ba.append_word_rev('123', 3);
    /// assert!(ba == "321");
    /// ```
    fn append_word_rev(ref self: ByteArray, word: felt252, len: usize) {
        let mut index = 0;

        let u256 { low, high } = word.into();
        let low_part_limit = min(len, BYTES_IN_U128);
        loop {
            if index == low_part_limit {
                break;
            }
            self.append_byte(core::bytes_31::get_lsb(split_u128(low, index).high));
            index += 1;
        }
        if low_part_limit == BYTES_IN_U128 {
            let mut index_in_high_part = 0;
            let high_part_len = len - BYTES_IN_U128;
            loop {
                if index_in_high_part == high_part_len {
                    break;
                }
                self
                    .append_byte(
                        core::bytes_31::get_lsb(split_u128(high, index_in_high_part).high),
                    );
                index_in_high_part += 1;
            }
        }
    }

    // === Helpers ===

    /// Appends a single word of `len` bytes to the end of the `ByteArray`, assuming there
    /// is enough space in the pending word (`self.pending_word_len + len < BYTES_IN_BYTES31`).
    ///
    /// `word` is of type `felt252` but actually represents a `bytes31`.
    /// It is represented as a `felt252` to improve performance of building the `ByteArray`.
    #[inline]
    fn append_word_fits_into_pending(ref self: ByteArray, word: felt252, len: usize) {
        if self.pending_word_len == 0 {
            // len < BYTES_IN_BYTES31
            self.pending_word = word;
            self.pending_word_len = len;
            return;
        }

        self.pending_word = word + self.pending_word * one_shift_left_bytes_felt252(len);
        self.pending_word_len += len;
    }

    /// Appends a single word to the end of `self`, given that `0 < split_index < 16`.
    ///
    /// `split_index` is the number of bytes left in `self.pending_word` after this function.
    /// This is the index of the split (LSB's index is 0).
    ///
    /// Note: this function doesn't update the new pending length of `self`. It's the caller's
    /// responsibility.
    #[inline]
    fn append_split_index_lt_16(ref self: ByteArray, word: felt252, split_index: usize) {
        let u256 { low, high } = word.into();

        let low_result = split_u128(low, split_index);
        let left = high.into() * one_shift_left_bytes_u128(BYTES_IN_U128 - split_index).into()
            + low_result.high.into();

        self.append_split(left, low_result.low.into());
    }

    /// Appends a single word to the end of `self`, given that the index of splitting `word` is
    /// exactly 16.
    ///
    /// `split_index` is the number of bytes left in `self.pending_word` after this function.
    /// This is the index of the split (LSB's index is 0).
    ///
    /// Note: this function doesn't update the new pending length of `self`. It's the caller's
    /// responsibility.
    #[inline]
    fn append_split_index_16(ref self: ByteArray, word: felt252) {
        let u256 { low, high } = word.into();
        self.append_split(high.into(), low.into());
    }

    /// Appends a single word to the end of `self`, given that the index of splitting `word` is >
    /// 16.
    ///
    /// `split_index` is the number of bytes left in `self.pending_word` after this function.
    /// This is the index of the split (LSB's index is 0).
    ///
    /// Note: this function doesn't update the new pending length of `self`. It's the caller's
    /// responsibility.
    #[inline]
    fn append_split_index_gt_16(ref self: ByteArray, word: felt252, split_index: usize) {
        let u256 { low, high } = word.into();

        let high_result = split_u128(high, split_index - BYTES_IN_U128);
        let right = high_result.low.into() * POW_2_128 + low.into();

        self.append_split(high_result.high.into(), right);
    }

    /// A helper function to append a remainder to `self`, by:
    /// 1. completing `self.pending_word` to a full word using `complete_full_word`, assuming it's
    ///    validly convertible to a `bytes31` of length exactly `BYTES_IN_BYTES31 -
    ///    self.pending_word_len`.
    /// 2. Setting `self.pending_word` to `new_pending`.
    ///
    /// Note: this function doesn't update the new pending length of `self`. It's the caller's
    /// responsibility.
    #[inline]
    fn append_split(ref self: ByteArray, complete_full_word: felt252, new_pending: felt252) {
        let to_append = complete_full_word
            + self.pending_word
                * one_shift_left_bytes_felt252(BYTES_IN_BYTES31 - self.pending_word_len);
        self.data.append(to_append.try_into().unwrap());
        self.pending_word = new_pending;
    }
}

impl ByteArrayAdd of Add<ByteArray> {
    /// Concatenates two `ByteArray` and returns the resulting `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// let ba: ByteArray = "1";
    /// let other_ba: ByteArray = "2";
    /// let result = ba + other_ba;
    /// assert!(result == "12")
    /// ```
    #[inline]
    fn add(lhs: ByteArray, rhs: ByteArray) -> ByteArray {
        ByteArrayTrait::concat(@lhs, @rhs)
    }
}

#[feature("deprecated-op-assign-traits")]
impl ByteArrayAddEq of crate::traits::AddEq<ByteArray> {
    /// Appends a `ByteArray` to another `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut ba: ByteArray = "1";
    /// let other_ba: ByteArray = "2";
    /// ba += other_ba;
    /// assert!(ba == "12");
    /// ```
    #[inline]
    fn add_eq(ref self: ByteArray, other: ByteArray) {
        self.append(@other);
    }
}

#[feature("deprecated-index-traits")]
pub(crate) impl ByteArrayIndexView of crate::traits::IndexView<ByteArray, usize, u8> {
    fn index(self: @ByteArray, index: usize) -> u8 {
        self.at(index).expect('Index out of bounds')
    }
}

// TODO: Implement a more efficient version of this iterator.
/// An iterator struct over a ByteArray.
#[derive(Drop, Clone)]
pub struct ByteArrayIter {
    ba: ByteArray,
    current_index: crate::ops::RangeIterator<usize>,
}

impl ByteArrayIterator of crate::iter::Iterator<ByteArrayIter> {
    type Item = u8;
    fn next(ref self: ByteArrayIter) -> Option<u8> {
        self.ba.at(self.current_index.next()?)
    }
}

impl ByteArrayIntoIterator of crate::iter::IntoIterator<ByteArray> {
    type IntoIter = ByteArrayIter;
    #[inline]
    fn into_iter(self: ByteArray) -> Self::IntoIter {
        ByteArrayIter { current_index: (0..self.len()).into_iter(), ba: self }
    }
}

impl ByteArrayFromIterator of crate::iter::FromIterator<ByteArray, u8> {
    fn from_iter<
        I,
        impl IntoIter: IntoIterator<I>,
        +core::metaprogramming::TypeEqual<IntoIter::Iterator::Item, u8>,
        +Destruct<IntoIter::IntoIter>,
        +Destruct<I>,
    >(
        iter: I,
    ) -> ByteArray {
        let mut ba = Default::default();
        for byte in iter {
            ba.append_byte(byte);
        }
        ba
    }
}
