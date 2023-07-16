use traits::{Into, TryInto};
use bytes_31::{U128IntoBytes31, U8IntoBytes31};
use array::{ArrayTrait, SpanTrait};
use option::OptionTrait;
use clone::Clone;
use integer::{u128_safe_divmod, u128_to_felt252, u8_to_felt252, u256_from_felt252};
use bytes_31::{
    BYTES_IN_BYTES31, bytes31_try_from_felt252, one_shift_left_bytes_felt252,
    one_shift_left_bytes_u128, split_bytes31
};
use zeroable::NonZeroIntoImpl;

const BYTES_IN_U128: u8 = 16;
// TODO(yuval): change to `BYTES_IN_BYTES31 - 1` once consteval_int supports non-literals.
const BYTES_IN_BYTES31_MINUS_ONE: u8 = consteval_int!(31 - 1);

// TODO(yuval): don't allow creation of invalid ByteArray?
#[derive(Drop, Clone)]
struct ByteArray {
    // Full "words" of 31 bytes each.
    data: Array<bytes31>,
    // This felt252 actually represents a bytes31, with < 31 bytes.
    // It is represented as a felt252 to improve performance of building the byte array.
    // The number of bytes in here is specified in `pending_word_len`.
    pending_word: felt252,
    // Should be in range [0, 30].
    pending_word_len: u8,
}

impl ByteArrayDefault of Default<ByteArray> {
    fn default() -> ByteArray {
        ByteArray { data: Default::default(), pending_word: 0, pending_word_len: 0 }
    }
}

use debug::PrintTrait;
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
    fn append_word(ref self: ByteArray, word: felt252, len: u8) {
        if len == 0 {
            return;
        }

        if self.pending_word_len + len >= BYTES_IN_BYTES31 {
            let split_index = BYTES_IN_BYTES31 - self.pending_word_len;
            let (first, second) = split_bytes31(:word, :len, index: split_index);
            let to_append = first * one_shift_left_bytes_felt252(self.pending_word_len)
                + self.pending_word;
            self.data.append(to_append.try_into().unwrap());
            self.pending_word = second;
            self.pending_word_len = len - split_index;
            return;
        }

        self.append_word_fits_into_pending(word, len);
    }

    // Appends a single word of `len` bytes to the end of the ByteArray, assuming there
    // is enough space in the pending word (`self.pending_word_len + len < BYTES_IN_BYTES31`).
    //
    // `word` is of type felt252 but actually represents a bytes31.
    // It is represented as a felt252 to improve performance of building the byte array.
    fn append_word_fits_into_pending(ref self: ByteArray, word: felt252, len: u8) {
        if self.pending_word_len == 0 {
            if len == BYTES_IN_BYTES31 {
                self.data.append(word.try_into().unwrap());
                return;
            } else {
                self.pending_word = word;
                self.pending_word_len = len;
                return;
            }
        }

        let to_append = word * one_shift_left_bytes_felt252(self.pending_word_len)
            + self.pending_word;

        self.pending_word = to_append;
        self.pending_word_len += len;
    }

    // Appends a byte array to the end of `self`.
    fn append(ref self: ByteArray, mut other: @ByteArray) {
        let mut other_data = other.data.span();

        // TODO(yg): this is a workaround for a bug we need to resolve. After resolving, change all
        // self_* to self.*.
        let ByteArray{data: mut self_data,
        pending_word: mut self_pending_word,
        pending_word_len: mut self_pending_word_len } =
            self;

        if self_pending_word_len == 0 {
            loop {
                match other_data.pop_front() {
                    Option::Some(current_word) => {
                        self_data.append(*current_word);
                    },
                    Option::None(_) => {
                        break;
                    }
                };
            };
            self_pending_word = *other.pending_word;
            self_pending_word_len = *other.pending_word_len;

            self = ByteArray {
                data: self_data,
                pending_word: self_pending_word,
                pending_word_len: self_pending_word_len
            };
            return;
        }

        self = ByteArray {
            data: self_data,
            pending_word: self_pending_word,
            pending_word_len: self_pending_word_len
        };

        let total_pending_bytes = self.pending_word_len + *other.pending_word_len;

        // split_index in [1, 30].
        let split_index = BYTES_IN_BYTES31 - self.pending_word_len;
        if split_index == BYTES_IN_U128 {
            loop {
                match other_data.pop_front() {
                    Option::Some(current_word) => {
                        self.append_split_index_medium((*current_word).into());
                    },
                    Option::None(_) => {
                        break;
                    }
                };
            };

            if total_pending_bytes >= BYTES_IN_BYTES31 {
                self.append_split_index_medium(*other.pending_word);
                self.pending_word_len = total_pending_bytes - BYTES_IN_BYTES31;
            } else {
                self.append_word_fits_into_pending(*other.pending_word, *other.pending_word_len);
            }
            return;
        }

        if split_index < BYTES_IN_U128 {
            loop {
                match other_data.pop_front() {
                    Option::Some(current_word) => {
                        self.append_split_index_short((*current_word).into(), split_index);
                    },
                    Option::None(_) => {
                        break;
                    }
                };
            };

            if total_pending_bytes >= BYTES_IN_BYTES31 {
                self.append_split_index_short(*other.pending_word, split_index);
                self.pending_word_len = total_pending_bytes - BYTES_IN_BYTES31;
            } else {
                self.append_word_fits_into_pending(*other.pending_word, *other.pending_word_len);
            }
            return;
        }

        // split_index > BYTES_IN_U128
        loop {
            match other_data.pop_front() {
                Option::Some(current_word) => {
                    self.append_split_index_long((*current_word).into(), split_index);
                },
                Option::None(_) => {
                    break;
                }
            };
        };

        if total_pending_bytes >= BYTES_IN_BYTES31 {
            self.append_split_index_long(*other.pending_word, split_index);
            self.pending_word_len = total_pending_bytes - BYTES_IN_BYTES31;
        } else {
            self.append_word_fits_into_pending(*other.pending_word, *other.pending_word_len);
        }
    }

    // Concatenates two byte arrays and returns the result.
    fn concat(left: @ByteArray, right: @ByteArray) -> ByteArray {
        let mut result: ByteArray = left.clone();
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

        let new_pending = self.pending_word.into()
            + byte.into() * one_shift_left_bytes_felt252(self.pending_word_len);

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

    // Appends a single word to the end of `self`, given that the index of splitting `word` is < 16.
    //
    // Note: this function doesn't update the new pending length of self. It's the caller's
    // responsibility.
    fn append_split_index_short(ref self: ByteArray, word: felt252, split_index: u8) {
        let u256{low, high } = u256_from_felt252(word);

        let (low_quotient, low_remainder) = u128_safe_divmod(
            low, one_shift_left_bytes_u128(split_index).try_into().unwrap()
        );
        let right = high.into() * one_shift_left_bytes_felt252(BYTES_IN_U128 - split_index)
            + low_quotient.into();

        self.append_split(low_remainder.into(), right);
    }

    // Appends a single word to the end of `self`, given that the index of splitting `word` is
    // exactly 16.
    //
    // Note: this function doesn't update the new pending length of self. It's the caller's
    // responsibility.
    fn append_split_index_medium(ref self: ByteArray, word: felt252) {
        let u256{low, high } = u256_from_felt252(word);
        self.append_split(low.into(), high.into());
    }

    // Appends a single word to the end of `self`, given that the index of splitting `word` is > 16.
    //
    // Note: this function doesn't update the new pending length of self. It's the caller's
    // responsibility.
    fn append_split_index_long(ref self: ByteArray, word: felt252, split_index: u8) {
        let u256{low, high } = u256_from_felt252(word);

        let (high_quotient, high_remainder) = u128_safe_divmod(
            high, one_shift_left_bytes_u128(split_index - BYTES_IN_U128).try_into().unwrap()
        );
        let left = high_remainder.into() * POW_2_128 + low.into();

        self.append_split(left, high_quotient.into());
    }

    // A helper function to append a remainder to self, by:
    // 1. completing `self.pending_word` to a full word using `complete_full_word`, assuming it's
    //    validly convertible to a `bytes31` of length exactly `BYTES_IN_BYTES31 -
    //    self.pending_word_len`.
    // 2. Setting `self.pending_word` to `new_pending`.
    //
    // Note: this function doesn't update the new pending length of self. It's the caller's
    // responsibility.
    fn append_split(ref self: ByteArray, complete_full_word: felt252, new_pending: felt252) {
        let to_append = complete_full_word * one_shift_left_bytes_felt252(self.pending_word_len)
            + self.pending_word;
        self.data.append(to_append.try_into().unwrap());
        self.pending_word = new_pending;
    }
}

