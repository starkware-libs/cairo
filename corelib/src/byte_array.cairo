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

        // The split index is the number of bytes left for the next word (new pending_word of the
        // modified ByteArray).
        let split_index = if let Some(split_index) =
            crate::num::traits::CheckedSub::checked_sub(total_pending_bytes, BYTES_IN_BYTES31) {
            split_index
        } else {
            self.pending_word = word + self.pending_word * one_shift_left_bytes_felt252(len);
            self.pending_word_len = total_pending_bytes;
            return;
        };

        let shift_value = self.shift_value();

        if split_index == 0 {
            self.append_shifted(SplitToAddResult { to_add: word, remainder: 0 }, shift_value);
            self.pending_word_len = 0;
            return;
        }
        let word_as_u256 = word.into();
        let to_append = match split_info(split_index) {
            SplitInfo::Eq16(v) => v.split_u256(word_as_u256),
            SplitInfo::Lt16(v) => v.split_u256(word_as_u256),
            SplitInfo::Gt16(v) => v.split_u256(word_as_u256),
            SplitInfo::BadUserData => crate::panic_with_felt252('bad append len'),
        };
        self.append_shifted(to_append, shift_value);
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

        let shift_value = self.shift_value();
        // self.pending_word_len is in [1, 30]. This is the split index for all the full words of
        // `other`, as for each word, this is the number of bytes left for the next word.
        match split_info(self.pending_word_len) {
            SplitInfo::Eq16(v) => {
                while let Some(word) = other_data.pop_front() {
                    self.append_shifted(v.split_u256((*word).into()), shift_value);
                }
            },
            SplitInfo::Lt16(v) => {
                while let Some(word) = other_data.pop_front() {
                    self.append_shifted(v.split_u256((*word).into()), shift_value);
                }
            },
            SplitInfo::Gt16(v) => {
                while let Some(word) = other_data.pop_front() {
                    self.append_shifted(v.split_u256((*word).into()), shift_value);
                }
            },
            SplitInfo::BadUserData => crate::panic_with_felt252('bad append len'),
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
        self.append_bytes31(new_pending);
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
        while let Some(current_word) = data.pop_back() {
            result.append_word_rev((*current_word).into(), BYTES_IN_BYTES31);
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
}
/// Internal functions associated with the `ByteArray` type.
#[generate_trait]
impl InternalImpl of InternalTrait {
    /// Appends a split word to the end of `self`.
    ///
    /// `value` is the value to add to the byte array, including `to_add` and `remainder`.
    /// `shift_value` is the shift for `self.pending word`, before it is joined by the `to_add` part
    /// of `value`.
    ///
    /// Note: this function doesn't update the new pending length of `self`. It's the caller's
    /// responsibility.
    #[inline]
    fn append_shifted(ref self: ByteArray, value: SplitToAddResult, shift_value: felt252) {
        self.append_bytes31(value.to_add + self.pending_word * shift_value);
        self.pending_word = value.remainder;
    }

    /// The value to shift the current pending word to add the remaining bytes to it.
    #[inline]
    fn shift_value(ref self: ByteArray) -> felt252 {
        one_shift_left_bytes_felt252(BYTES_IN_BYTES31 - self.pending_word_len)
    }

    /// Appends a `felt252` value assumed to be `bytes31`.
    ///
    /// Will append an error value in cases of invalid usage in order to avoid panic code.
    fn append_bytes31(ref self: ByteArray, value: felt252) {
        const ON_ERR: bytes31 = 'BA_ILLEGAL_USAGE'_u128.into();
        self.data.append(value.try_into().unwrap_or(ON_ERR));
    }
}

/// The value for adding up to 31 bytes to the byte array.
struct SplitToAddResult {
    /// The value to complete the current word.
    to_add: felt252,
    /// The value to update the pending word to, as it does not fit in the current word.
    remainder: felt252,
}

/// Information for splitting a felt252 into 2 parts at an index.
enum SplitInfo {
    /// The index is 16.
    Eq16: Eq16SplitInfo,
    /// The index is less than 16.
    Lt16: Lt16SplitInfo,
    /// The index is more than 16.
    Gt16: Gt16SplitInfo,
    /// Should never happen.
    BadUserData,
}

/// Helper struct for splitting a number at the 16 byte.
#[derive(Copy, Drop)]
struct Eq16SplitInfo {}

/// Helper struct for splitting a number at an index lower than the 16 byte.
#[derive(Copy, Drop)]
struct Lt16SplitInfo {
    /// The division value to extract the low word parts.
    low_div: NonZero<u128>,
    /// The shift value to mix the high word and the low word high part.
    high_shift: felt252,
}

/// Helper struct for splitting a number at an index greater than the 16 byte.
#[derive(Copy, Drop)]
struct Gt16SplitInfo {
    /// The division value to extract the high word parts.
    high_div: NonZero<u128>,
}

trait SplitValue<T> {
    /// Splits a u256 into a `SplitToAddResult` according to the info on the split index.
    fn split_u256(self: T, v: u256) -> SplitToAddResult;
}

impl Lt16SplitInfoSplitValue of SplitValue<Lt16SplitInfo> {
    fn split_u256(self: Lt16SplitInfo, v: u256) -> SplitToAddResult {
        let (low_high, low_low) = DivRem::div_rem(v.low, self.low_div);
        SplitToAddResult {
            to_add: v.high.into() * self.high_shift + low_high.into(), remainder: low_low.into(),
        }
    }
}

impl Eq16SplitInfoSplitValue of SplitValue<Eq16SplitInfo> {
    fn split_u256(self: Eq16SplitInfo, v: u256) -> SplitToAddResult {
        SplitToAddResult { to_add: v.high.into(), remainder: v.low.into() }
    }
}

impl Gt16SplitInfoSplitValue of SplitValue<Gt16SplitInfo> {
    fn split_u256(self: Gt16SplitInfo, v: u256) -> SplitToAddResult {
        let (high_high, high_low) = DivRem::div_rem(v.high, self.high_div);
        SplitToAddResult {
            to_add: high_high.into(), remainder: high_low.into() * POW_2_128 + v.low.into(),
        }
    }
}

/// Extracts the split info from the given index.
fn split_info(split_index: usize) -> SplitInfo {
    match split_index {
        1 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x100, high_shift: 0x1000000000000000000000000000000 },
        ),
        2 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x10000, high_shift: 0x10000000000000000000000000000 },
        ),
        3 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x1000000, high_shift: 0x100000000000000000000000000 },
        ),
        4 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x100000000, high_shift: 0x1000000000000000000000000 },
        ),
        5 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x10000000000, high_shift: 0x10000000000000000000000 },
        ),
        6 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x1000000000000, high_shift: 0x100000000000000000000 },
        ),
        7 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x100000000000000, high_shift: 0x1000000000000000000 },
        ),
        8 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x10000000000000000, high_shift: 0x10000000000000000 },
        ),
        9 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x1000000000000000000, high_shift: 0x100000000000000 },
        ),
        10 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x100000000000000000000, high_shift: 0x1000000000000 },
        ),
        11 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x10000000000000000000000, high_shift: 0x10000000000 },
        ),
        12 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x1000000000000000000000000, high_shift: 0x100000000 },
        ),
        13 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x100000000000000000000000000, high_shift: 0x1000000 },
        ),
        14 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x10000000000000000000000000000, high_shift: 0x10000 },
        ),
        15 => SplitInfo::Lt16(
            Lt16SplitInfo { low_div: 0x1000000000000000000000000000000, high_shift: 0x100 },
        ),
        16 => SplitInfo::Eq16(Eq16SplitInfo {}),
        17 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x100 }),
        18 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x10000 }),
        19 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x1000000 }),
        20 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x100000000 }),
        21 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x10000000000 }),
        22 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x1000000000000 }),
        23 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x100000000000000 }),
        24 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x10000000000000000 }),
        25 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x1000000000000000000 }),
        26 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x100000000000000000000 }),
        27 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x10000000000000000000000 }),
        28 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x1000000000000000000000000 }),
        29 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x100000000000000000000000000 }),
        30 => SplitInfo::Gt16(Gt16SplitInfo { high_div: 0x10000000000000000000000000000 }),
        0 | _ => SplitInfo::BadUserData,
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
    current_index: IntoIterator::<crate::ops::Range<usize>>::IntoIter,
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
