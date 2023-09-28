use array::{ArrayTrait, SpanTrait};
use bytes_31::{
    BYTES_IN_BYTES31, Bytes31Trait, one_shift_left_bytes_felt252, one_shift_left_bytes_u128,
    POW_2_128, POW_2_8, U128IntoBytes31, U8IntoBytes31
};
use clone::Clone;
use cmp::min;
use integer::{u128_safe_divmod, U32TryIntoNonZero};
use option::OptionTrait;
use traits::{Into, TryInto};
use zeroable::NonZeroIntoImpl;

const BYTES_IN_U128: usize = 16;
// TODO(yuval): change to `BYTES_IN_BYTES31 - 1` once consteval_int supports non-literals.
const BYTES_IN_BYTES31_MINUS_ONE: usize = consteval_int!(31 - 1);

// TODO(yuval): don't allow creation of invalid ByteArray?
#[derive(Drop, Clone, PartialEq)]
struct ByteArray {
    // Full "words" of 31 bytes each. The first byte of each word in the byte array
    // is the most significant byte in the word.
    data: Array<bytes31>,
    // This felt252 actually represents a bytes31, with < 31 bytes.
    // It is represented as a felt252 to improve performance of building the byte array.
    // The number of bytes in here is specified in `pending_word_len`.
    // The first byte is the most significant byte among the `pending_word_len` bytes in the word.
    pending_word: felt252,
    // Should be in range [0, 30].
    pending_word_len: usize,
}

impl ByteArrayStringLiteral of string::StringLiteral<ByteArray>;

impl ByteArrayDefault of Default<ByteArray> {
    fn default() -> ByteArray {
        ByteArray { data: Default::default(), pending_word: 0, pending_word_len: 0 }
    }
}

#[generate_trait]
impl ByteArrayImpl of ByteArrayTrait {
    // TODO(yuval): add a `new` function for initialization.

    // Appends a single word of `len` bytes to the end of the ByteArray.
    // Note: this function assumes that:
    // 1. `word` could be validly converted to a `bytes31` which has no more than `len` bytes
    //    of data.
    // 2. len <= BYTES_IN_BYTES31.
    // If these assumptions are not met, it can corrupt the ByteArray. Thus, this should be a
    // private function. We could add masking/assertions but it would be more expensive.
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
                        .unwrap()
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

    // Appends a byte array to the end of `self`.
    fn append(ref self: ByteArray, mut other: @ByteArray) {
        let mut other_data = other.data.span();

        if self.pending_word_len == 0 {
            loop {
                match other_data.pop_front() {
                    Option::Some(current_word) => { self.data.append(*current_word); },
                    Option::None => { break; }
                };
            };
            self.pending_word = *other.pending_word;
            self.pending_word_len = *other.pending_word_len;
            return;
        }

        // self.pending_word_len is in [1, 30]. This is the split index for all the full words of
        // `other`, as for each word, this is the number of bytes left for the next word.
        if self.pending_word_len == BYTES_IN_U128 {
            loop {
                match other_data.pop_front() {
                    Option::Some(current_word) => {
                        self.append_split_index_16((*current_word).into());
                    },
                    Option::None => { break; }
                };
            };
        } else if self.pending_word_len < BYTES_IN_U128 {
            loop {
                match other_data.pop_front() {
                    Option::Some(current_word) => {
                        self
                            .append_split_index_lt_16(
                                (*current_word).into(), self.pending_word_len
                            );
                    },
                    Option::None => { break; }
                };
            };
        } else {
            // self.pending_word_len > BYTES_IN_U128
            loop {
                match other_data.pop_front() {
                    Option::Some(current_word) => {
                        self
                            .append_split_index_gt_16(
                                (*current_word).into(), self.pending_word_len
                            );
                    },
                    Option::None => { break; }
                };
            };
        }

        // Add the pending word of `other`.
        self.append_word(*other.pending_word, *other.pending_word_len);
    }

    // Concatenates two byte arrays and returns the result.
    fn concat(left: @ByteArray, right: @ByteArray) -> ByteArray {
        let mut result = left.clone();
        result.append(right);
        result
    }

    // Appends a single byte to the end of `self`.
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

    fn len(self: @ByteArray) -> usize {
        self.data.len() * BYTES_IN_BYTES31.into() + (*self.pending_word_len).into()
    }

    // Returns the byte at the given index, or None if the index is out of bounds.
    fn at(self: @ByteArray, index: usize) -> Option<u8> {
        let (word_index, index_in_word) = DivRem::div_rem(
            index, BYTES_IN_BYTES31.try_into().unwrap()
        );

        let data_len = self.data.len();
        if word_index == data_len {
            // Index is in pending word.
            if index_in_word >= *self.pending_word_len {
                return Option::None;
            }
            // index_in_word is from MSB, we need index from LSB.
            let index_from_lsb = *self.pending_word_len - 1 - index_in_word;
            let pending_bytes31: bytes31 = (*self.pending_word).try_into().unwrap();
            return Option::Some(pending_bytes31.at(index_from_lsb));
        }

        if word_index > data_len {
            return Option::None;
        }

        // index_in_word is from MSB, we need index from LSB.
        let index_from_lsb = BYTES_IN_BYTES31 - 1 - index_in_word;
        Option::Some(self.data.at(word_index).at(index_from_lsb))
    }

    /// Returns a ByteArray with the reverse order of `self`.
    fn rev(self: @ByteArray) -> ByteArray {
        let mut result = Default::default();

        result.append_word_rev(*self.pending_word, *self.pending_word_len);

        let mut data = self.data.span();
        loop {
            match data.pop_back() {
                Option::Some(current_word) => {
                    result.append_word_rev((*current_word).into(), BYTES_IN_BYTES31);
                },
                Option::None => { break; }
            };
        };
        result
    }

    /// Appends the reverse of the given word to the end `self`.
    /// Assumptions:
    /// 1. len < 31
    /// 2. word is validly convertible to bytes31 of length `len`.
    fn append_word_rev(ref self: ByteArray, word: felt252, len: usize) {
        let mut index = 0;

        let u256{low, high } = word.into();
        let low_part_limit = min(len, BYTES_IN_U128);
        loop {
            if index == low_part_limit {
                break;
            }
            let curr_byte_as_u128 = (low / one_shift_left_bytes_u128(index)) % POW_2_8;

            self.append_byte(curr_byte_as_u128.try_into().unwrap());
            index += 1;
        };
        if low_part_limit == BYTES_IN_U128 {
            let mut index_in_high_part = 0;
            let high_part_len = len - BYTES_IN_U128;
            loop {
                if index_in_high_part == high_part_len {
                    break;
                }
                let curr_byte_as_u128 = (high
                    / one_shift_left_bytes_u128(index_in_high_part)) % POW_2_8;

                self.append_byte(curr_byte_as_u128.try_into().unwrap());
                index_in_high_part += 1;
            }
        }
    }

    // === Helpers ===

    // Appends a single word of `len` bytes to the end of the ByteArray, assuming there
    // is enough space in the pending word (`self.pending_word_len + len < BYTES_IN_BYTES31`).
    //
    // `word` is of type felt252 but actually represents a bytes31.
    // It is represented as a felt252 to improve performance of building the byte array.
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

    // Appends a single word to the end of `self`, given that `0 < split_index < 16`.
    //
    // `split_index` is the number of bytes left in `self.pending_word` after this function. This is
    // the index of the split (LSB's index is 0).
    //
    // Note: this function doesn't update the new pending length of self. It's the caller's
    // responsibility.
    #[inline]
    fn append_split_index_lt_16(ref self: ByteArray, word: felt252, split_index: usize) {
        let u256{low, high } = word.into();

        let (low_quotient, low_remainder) = u128_safe_divmod(
            low, one_shift_left_bytes_u128(split_index).try_into().unwrap()
        );
        let left = high.into() * one_shift_left_bytes_u128(BYTES_IN_U128 - split_index).into()
            + low_quotient.into();

        self.append_split(left, low_remainder.into());
    }

    // Appends a single word to the end of `self`, given that the index of splitting `word` is
    // exactly 16.
    //
    // `split_index` is the number of bytes left in `self.pending_word` after this function. This is
    // the index of the split (LSB's index is 0).
    //
    // Note: this function doesn't update the new pending length of self. It's the caller's
    // responsibility.
    #[inline]
    fn append_split_index_16(ref self: ByteArray, word: felt252) {
        let u256{low, high } = word.into();
        self.append_split(high.into(), low.into());
    }

    // Appends a single word to the end of `self`, given that the index of splitting `word` is > 16.
    //
    // `split_index` is the number of bytes left in `self.pending_word` after this function. This is
    // the index of the split (LSB's index is 0).
    //
    // Note: this function doesn't update the new pending length of self. It's the caller's
    // responsibility.
    #[inline]
    fn append_split_index_gt_16(ref self: ByteArray, word: felt252, split_index: usize) {
        let u256{low, high } = word.into();

        let (high_quotient, high_remainder) = u128_safe_divmod(
            high, one_shift_left_bytes_u128(split_index - BYTES_IN_U128).try_into().unwrap()
        );
        let right = high_remainder.into() * POW_2_128 + low.into();

        self.append_split(high_quotient.into(), right);
    }

    // A helper function to append a remainder to self, by:
    // 1. completing `self.pending_word` to a full word using `complete_full_word`, assuming it's
    //    validly convertible to a `bytes31` of length exactly `BYTES_IN_BYTES31 -
    //    self.pending_word_len`.
    // 2. Setting `self.pending_word` to `new_pending`.
    //
    // Note: this function doesn't update the new pending length of self. It's the caller's
    // responsibility.
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
    #[inline]
    fn add(lhs: ByteArray, rhs: ByteArray) -> ByteArray {
        ByteArrayTrait::concat(@lhs, @rhs)
    }
}
impl ByteArrayAddEq of AddEq<ByteArray> {
    #[inline]
    fn add_eq(ref self: ByteArray, other: ByteArray) {
        self.append(@other);
    }
}

impl ByteArrayIndexView of IndexView<ByteArray, usize, u8> {
    fn index(self: @ByteArray, index: usize) -> u8 {
        self.at(index).expect('Index out of bounds')
    }
}
