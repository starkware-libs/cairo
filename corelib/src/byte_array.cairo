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

use crate::array::{ArrayTrait, Span, SpanTrait};
use crate::bytes_31::split_bytes31;
#[allow(unused_imports)]
use crate::bytes_31::{
    BYTES_IN_BYTES31, Bytes31Trait, POW_2_128, POW_2_8, U128IntoBytes31, U8IntoBytes31,
    one_shift_left_bytes_felt252, one_shift_left_bytes_u128, split_u128, u8_at_u256,
};
use crate::clone::Clone;
use crate::cmp::min;
#[allow(unused_imports)]
use crate::integer::{U32TryIntoNonZero, u128_safe_divmod};
#[feature("bounded-int-utils")]
use crate::internal::bounded_int::{self, BoundedInt, downcast, upcast};
#[allow(unused_imports)]
use crate::serde::Serde;
use crate::traits::{Into, TryInto};
#[allow(unused_imports)]
use crate::zeroable::NonZeroIntoImpl;

/// The number of bytes in [`ByteArray::pending_word`].
type Bytes31Index = BoundedInt<0, { BYTES_IN_BYTES31_MINUS_ONE.into() }>;

/// A magic constant for identifying serialization of `ByteArray` variables. An array of `felt252`
/// with this magic value as one of the `felt252` indicates that you should expect right after it a
/// serialized `ByteArray`. This is currently used mainly for prints and panics.
pub const BYTE_ARRAY_MAGIC: felt252 =
    0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3;
const BYTES_IN_U128: usize = 16;
const BYTES_IN_BYTES31_MINUS_ONE: usize = BYTES_IN_BYTES31 - 1;
const BYTES_IN_BYTES31_NONZERO: NonZero<usize> = BYTES_IN_BYTES31.try_into().unwrap();

// TODO(yuval): don't allow creation of invalid ByteArray?
/// Byte array type.
#[derive(Drop, Clone, PartialEq, Serde)]
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
    pub(crate) pending_word_len: Bytes31Index,
}
impl ByteArrayDefault of Default<ByteArray> {
    fn default() -> ByteArray {
        ByteArray { data: Default::default(), pending_word: 0, pending_word_len: 0 }
    }
}

pub(crate) impl ByteArrayStringLiteral of crate::string::StringLiteral<ByteArray>;

/// Functions associated with the `ByteArray` type.
#[generate_trait]
pub impl ByteArrayImpl of ByteArrayTrait {
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
        let crate::internal::OptionRev::Some(len) = bounded_int::trim_min(len) else {
            return;
        };
        match downcast(len) {
            Some(len) => self.append_word_ex(word, len),
            None => crate::panic_with_felt252('bad append len'),
        }
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
        self.append_from_parts(other.data.span(), *other.pending_word, *other.pending_word_len);
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

        if let Some(pending_word_len) = helpers::byte31_index_inc(self.pending_word_len) {
            self.pending_word = new_pending;
            self.pending_word_len = pending_word_len;
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
        helpers::calc_bytearray_len(self)
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
        let (word_index, index_in_word) = len_parts(index);
        let index_in_word: usize = upcast(index_in_word);
        if word_index == self.data.len() {
            // Index is in pending word.
            if index_in_word >= upcast(*self.pending_word_len) {
                return None;
            }
            // index_in_word is from MSB, we need index from LSB.
            return Some(
                u8_at_u256(
                    (*self.pending_word).into(), upcast(*self.pending_word_len) - 1 - index_in_word,
                ),
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

        result.append_word_rev(*self.pending_word, upcast(*self.pending_word_len));

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
    /// Internal implementation of `ByteArrayTrait::append_word`.
    fn append_word_ex(ref self: ByteArray, word: felt252, len: BoundedInt<1, 31>) {
        // The split index is the number of bytes left for the next word (new pending_word of the
        // modified ByteArray).
        let split_index = match helpers::append_word_info(self.pending_word_len, len) {
            helpers::AppendWordInfo::InPending(pending_word_len) => {
                self.pending_word = word
                    + self.pending_word * one_shift_left_bytes_felt252(upcast(len));
                self.pending_word_len = pending_word_len;
                return;
            },
            helpers::AppendWordInfo::FilledPending(split_index) => split_index,
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
        match self.pending_word_len {
            30 => 0x100,
            29 => 0x10000,
            28 => 0x1000000,
            27 => 0x100000000,
            26 => 0x10000000000,
            25 => 0x1000000000000,
            24 => 0x100000000000000,
            23 => 0x10000000000000000,
            22 => 0x1000000000000000000,
            21 => 0x100000000000000000000,
            20 => 0x10000000000000000000000,
            19 => 0x1000000000000000000000000,
            18 => 0x100000000000000000000000000,
            17 => 0x10000000000000000000000000000,
            16 => 0x1000000000000000000000000000000,
            15 => 0x100000000000000000000000000000000,
            14 => 0x10000000000000000000000000000000000,
            13 => 0x1000000000000000000000000000000000000,
            12 => 0x100000000000000000000000000000000000000,
            11 => 0x10000000000000000000000000000000000000000,
            10 => 0x1000000000000000000000000000000000000000000,
            9 => 0x100000000000000000000000000000000000000000000,
            8 => 0x10000000000000000000000000000000000000000000000,
            7 => 0x1000000000000000000000000000000000000000000000000,
            6 => 0x100000000000000000000000000000000000000000000000000,
            5 => 0x10000000000000000000000000000000000000000000000000000,
            4 => 0x1000000000000000000000000000000000000000000000000000000,
            3 => 0x100000000000000000000000000000000000000000000000000000000,
            2 => 0x10000000000000000000000000000000000000000000000000000000000,
            1 => 0x1000000000000000000000000000000000000000000000000000000000000,
            0 => 0x100000000000000000000000000000000000000000000000000000000000000,
            _ => crate::panic_with_felt252('unreachable'),
        }
    }

    /// Appends a `felt252` value assumed to be `bytes31`.
    ///
    /// Will append an error value in cases of invalid usage in order to avoid panic code.
    fn append_bytes31(ref self: ByteArray, value: felt252) {
        const ON_ERR: bytes31 = 'BA_ILLEGAL_USAGE'_u128.into();
        self.data.append(value.try_into().unwrap_or(ON_ERR));
    }

    /// Append `data`` span and `pending_word` fields to the byte array.
    #[inline]
    fn append_from_parts(
        ref self: ByteArray,
        mut data: Span<bytes31>,
        pending_word: felt252,
        pending_word_len: Bytes31Index,
    ) {
        if self.pending_word_len == 0 {
            self.data.append_span(data);
            self.pending_word = pending_word;
            self.pending_word_len = pending_word_len;
            return;
        }

        let shift_value = self.shift_value();
        // self.pending_word_len is in [1, 30]. This is the split index for all the full words of
        // `other`, as for each word, this is the number of bytes left for the next word.
        match split_info(self.pending_word_len) {
            SplitInfo::Eq16(v) => {
                while let Some(word) = data.pop_front() {
                    self.append_shifted(v.split_u256((*word).into()), shift_value);
                }
            },
            SplitInfo::Lt16(v) => {
                while let Some(word) = data.pop_front() {
                    self.append_shifted(v.split_u256((*word).into()), shift_value);
                }
            },
            SplitInfo::Gt16(v) => {
                while let Some(word) = data.pop_front() {
                    self.append_shifted(v.split_u256((*word).into()), shift_value);
                }
            },
            SplitInfo::BadUserData => crate::panic_with_felt252('bad append len'),
        }
        // Add the pending word of `other`.
        if let crate::internal::OptionRev::Some(pending_word_len) =
            bounded_int::trim_min::<_, helpers::TrimMinBytes31Index>(pending_word_len) {
            self.append_word_ex(pending_word, upcast(pending_word_len));
        }
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
fn split_info(split_index: Bytes31Index) -> SplitInfo {
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

/// A view into a contiguous collection of a string type.
/// Currently implemented only for `ByteArray`, but will soon be implemented for other string types.
/// `Span` implements the `Copy` and the `Drop` traits.
#[derive(Copy, Drop)]
pub struct ByteSpan {
    /// A span representing the array of all `bytes31` words in the byte-span, excluding the last
    /// bytes_31 word that is stored in [Self::last_word].
    /// Invariant: every byte stored in `data` is part of the span except for the bytes appearing
    /// before `first_char_start_offset` in the first word.
    data: Span<bytes31>,
    /// The offset of the first character in the first entry of [Self::data], for use in span
    /// slices. When data is empty, this offset applies to remainder_word instead.
    first_char_start_offset: Bytes31Index,
    /// Contains the final bytes of the span when the end is either not in memory or isn't aligned
    /// to a word boundary.
    /// It is represented as a `felt252` to improve performance of building the byte array, but
    /// represents a `bytes31`.
    /// The first byte is the most significant byte among the `pending_word_len` bytes in the word.
    remainder_word: felt252,
    /// The number of bytes in [Self::remainder_word].
    remainder_len: Bytes31Index,
}


#[generate_trait]
pub impl ByteSpanImpl of ByteSpanTrait {
    /// Returns the length of the `ByteSpan`.
    ///
    /// # Examples
    ///
    /// ```
    /// let ba: ByteArray = "byte array";
    /// let span = ba.span();
    /// let len = span.len();
    /// assert!(len == 10);
    /// ```
    #[must_use]
    fn len(self: ByteSpan) -> usize {
        helpers::calc_bytespan_len(self)
    }

    /// Returns `true` if the `ByteSpan` has a length of 0.
    ///
    /// # Examples
    ///
    /// ```
    /// let ba: ByteArray = "";
    /// let span = ba.span();
    /// assert!(span.is_empty());
    ///
    /// let ba2: ByteArray = "not empty";
    /// let span2 = ba2.span();
    /// assert!(!span2.is_empty());
    /// ```
    fn is_empty(self: ByteSpan) -> bool {
        // No need to check offsets: when `slice` consumes the span it returns `Default::default()`.
        self.remainder_len == 0 && self.data.len() == 0
    }

    /// Converts a `ByteSpan` into a `ByteArray`.
    /// The cast includes trimming the start_offset of the first word of the span (which is created
    /// when slicing).
    ///
    /// Note: creating `ByteArray.data` from `Span` requires allocating a new memory
    /// segment for the returned array, and *O*(*n*) operations to populate the array with the
    /// content of the span (see also `SpanIntoArray`).
    fn to_byte_array(mut self: ByteSpan) -> ByteArray {
        let remainder_len = upcast(self.remainder_len);
        let Some(first_word) = self.data.pop_front() else {
            // Slice is included entirely in the remainder word.
            let pending_word_len: usize = remainder_len - upcast(self.first_char_start_offset);
            let (pending_word, _) = split_bytes31(
                self.remainder_word, remainder_len, pending_word_len,
            );
            return ByteArray {
                data: array![], pending_word, pending_word_len: downcast(pending_word_len).unwrap(),
            };
        };

        let mut ba = Default::default();
        let n_bytes_to_append = BYTES_IN_BYTES31 - upcast(self.first_char_start_offset);
        let (first_word_no_offset, _) = split_bytes31(
            (*first_word).into(), BYTES_IN_BYTES31, n_bytes_to_append,
        );
        ba.append_word(first_word_no_offset, n_bytes_to_append);

        // Append the rest of the span parts, now that the first word was popped.
        ba.append_from_parts(self.data, self.remainder_word, upcast(self.remainder_len));
        ba
    }
}

impl ByteSpanDefault of Default<ByteSpan> {
    fn default() -> ByteSpan {
        ByteSpan {
            data: [].span(), first_char_start_offset: 0, remainder_word: 0, remainder_len: 0,
        }
    }
}

/// Trait for types that can be converted into a `ByteSpan`.
#[unstable(feature: "byte-span")]
pub trait ToByteSpanTrait<C> {
    #[must_use]
    /// Create a `ByteSpan` view object for the given type.
    fn span(self: @C) -> ByteSpan;
}

#[feature("byte-span")]
impl ByteArrayToByteSpan of ToByteSpanTrait<ByteArray> {
    fn span(self: @ByteArray) -> ByteSpan {
        ByteSpan {
            data: self.data.span(),
            first_char_start_offset: 0,
            remainder_word: *self.pending_word,
            remainder_len: downcast(self.pending_word_len).expect('In [0,30] by assumption'),
        }
    }
}

#[feature("byte-span")]
impl ByteSpanToByteSpan of ToByteSpanTrait<ByteSpan> {
    fn span(self: @ByteSpan) -> ByteSpan {
        *self
    }
}

mod helpers {
    use core::num::traits::Bounded;
    use crate::bytes_31::BYTES_IN_BYTES31;
    #[feature("bounded-int-utils")]
    use crate::internal::bounded_int::{
        self, AddHelper, BoundedInt, ConstrainHelper, MulHelper, SubHelper, UnitInt, downcast,
        upcast,
    };
    use super::{BYTES_IN_BYTES31_MINUS_ONE, ByteSpan, Bytes31Index};

    type BytesInBytes31Typed = UnitInt<{ BYTES_IN_BYTES31.into() }>;

    const U32_MAX_TIMES_B31: felt252 = Bounded::<u32>::MAX.into() * BYTES_IN_BYTES31.into();
    const BYTES_IN_BYTES31_UNIT_INT: BytesInBytes31Typed = downcast(BYTES_IN_BYTES31).unwrap();

    impl U32ByB31 of MulHelper<u32, BytesInBytes31Typed> {
        type Result = BoundedInt<0, U32_MAX_TIMES_B31>;
    }

    impl B30AddU32ByB31 of AddHelper<Bytes31Index, U32ByB31::Result> {
        type Result = BoundedInt<0, { BYTES_IN_BYTES31_MINUS_ONE.into() + U32_MAX_TIMES_B31 }>;
    }

    impl B30AddU32ByB31SubB30 of SubHelper<B30AddU32ByB31::Result, Bytes31Index> {
        type Result =
            BoundedInt<
                { -BYTES_IN_BYTES31_MINUS_ONE.into() },
                { BYTES_IN_BYTES31_MINUS_ONE.into() + U32_MAX_TIMES_B31 },
            >;
    }

    /// Calculates the length of a `ByteSpan` in bytes.
    pub fn calc_bytespan_len(span: ByteSpan) -> usize {
        let data_bytes = bounded_int::mul(span.data.len(), BYTES_IN_BYTES31_UNIT_INT);
        let span_bytes_unadjusted = bounded_int::add(span.remainder_len, data_bytes);
        let span_bytes = bounded_int::sub(span_bytes_unadjusted, span.first_char_start_offset);

        downcast(span_bytes).unwrap()
    }

    /// Calculates the length of a `ByteSpan` in bytes.
    pub fn calc_bytearray_len(arr: @ByteArray) -> usize {
        let data_bytes = bounded_int::mul(arr.data.len(), BYTES_IN_BYTES31_UNIT_INT);
        let arr_bytes = bounded_int::add(*arr.pending_word_len, data_bytes);

        downcast(arr_bytes).unwrap()
    }

    impl DivRemU32ByB31 of bounded_int::DivRemHelper<u32, BytesInBytes31Typed> {
        type DivT = BoundedInt<0, 0x8421084>;
        type RemT = Bytes31Index;
    }

    /// Returns the length of a `ByteArray` in full words and additional bytes in the pending word.
    pub fn len_parts(length: usize) -> (usize, Bytes31Index) {
        let (div, rem) = bounded_int::div_rem::<_, _, DivRemU32ByB31>(length, 31);
        (upcast(div), rem)
    }

    impl TrimMaxBytes31Index of bounded_int::TrimMaxHelper<Bytes31Index> {
        type Target = BoundedInt<0, 29>;
    }
    impl AddBytes31Index of AddHelper<BoundedInt<0, 29>, UnitInt<1>> {
        type Result = BoundedInt<1, 30>;
    }

    /// Increments the index by one, or returns `None` if not in range.
    pub fn byte31_index_inc(index: Bytes31Index) -> Option<Bytes31Index> {
        if let crate::internal::OptionRev::Some(trimmed) = bounded_int::trim_max(index) {
            Some(upcast(bounded_int::add(trimmed, 1)))
        } else {
            None
        }
    }

    /// The information about the new pending word length and the split index.
    pub enum AppendWordInfo {
        /// The new pending word length is less than 31, and fits in the current pending word.
        InPending: Bytes31Index,
        /// The new pending word length is greater than 31, and the current pending word is filled.
        FilledPending: Bytes31Index,
    }

    impl Bytes31IndexAdd of AddHelper<Bytes31Index, BoundedInt<1, 31>> {
        type Result = BoundedInt<1, 61>;
    }
    impl Constrain31Bytes31IndexAddResult of ConstrainHelper<Bytes31IndexAdd::Result, 31> {
        type LowT = BoundedInt<1, 30>;
        type HighT = BoundedInt<31, 61>;
    }
    impl Sub31Bytes31IndexAddResult of SubHelper<BoundedInt<31, 61>, BytesInBytes31Typed> {
        type Result = Bytes31Index;
    }

    /// Returns the information about the new pending word length and the split index.
    pub fn append_word_info(
        pending_bytes: Bytes31Index, new_word_bytes: BoundedInt<1, 31>,
    ) -> AppendWordInfo {
        let total_pending_bytes = bounded_int::add(pending_bytes, new_word_bytes);
        match bounded_int::constrain::<_, 31>(total_pending_bytes) {
            Ok(lt31) => AppendWordInfo::InPending(upcast(lt31)),
            Err(ge31) => AppendWordInfo::FilledPending(
                bounded_int::sub(ge31, BYTES_IN_BYTES31_UNIT_INT),
            ),
        }
    }

    /// Impl for trimming the minimum value of a `Bytes31Index`.
    pub impl TrimMinBytes31Index of bounded_int::TrimMinHelper<Bytes31Index> {
        type Target = BoundedInt<1, 30>;
    }
}
pub(crate) use helpers::len_parts;
