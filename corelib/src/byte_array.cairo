use traits::{Into, TryInto};
use bytes_31::{U128IntoBytes31, U8IntoBytes31};
use array::{ArrayTrait, SpanTrait};
use option::OptionTrait;
use clone::Clone;
use integer::{u128_safe_divmod, u128_to_felt252, u8_to_felt252, u256_from_felt252};
use bytes_31::{
    BYTES_IN_BYTES31, bytes31_try_from_felt252, one_shift_left_bytes_felt252,
    one_shift_left_bytes_u128, Bytes31Trait
};
use zeroable::NonZeroIntoImpl;

const BYTES_IN_U128: u8 = 16;

// TODO(yuval): don't allow creation of invalid ByteArray?
// TODO(yg): consider storing felts instead of bytes31s.
#[derive(Drop, Clone)]
struct ByteArray {
    // Full "words" of 31 bytes each.
    data: Array<bytes31>,
    // This felt252 actually represents a bytes31, with < 31 bytes.
    // It is represented as a felt252 to improve performance of building the byte array.
    // The number of bytes in here is specified in `pending_word_len`.
    pending_word: bytes31,
    // Should be in range [0, 30].
    pending_word_len: u8,
}

impl ByteArrayDefault of Default<ByteArray> {
    fn default() -> ByteArray {
        ByteArray {
            // TODO(yuval): change pending_word arg to literal.
            data: Default::default(), pending_word: bytes31_const::<0>(), pending_word_len: 0
        }
    }
}

use debug::PrintTrait;
#[generate_trait]
impl ByteArrayImpl of ByteArrayTrait {
    // TODO(yuval): add a `new` function for initialization.

    // Appends a single word of `num_bytes` bytes to the end of the ByteArray.
    // Note: this function assumes that:
    // 1. `word` has no more than `num_bytes` bytes of data.
    // 2. num_bytes <= BYTES_IN_BYTES31.
    // If these assumptions are not met, it can corrupt the ByteArray. Thus, this should be a
    // private function. We could add masking/assertions but it would be more expensive.
    fn append_word(ref self: ByteArray, word: bytes31, num_bytes: u8) {
        if num_bytes == 0 {
            return;
        }

        let num_total_pending_bytes = self.pending_word_len + num_bytes;
        if num_total_pending_bytes > BYTES_IN_BYTES31 {
            let split_index = BYTES_IN_BYTES31 - self.pending_word_len;
            let (first, second) = word.split_bytes31(len: num_bytes, index: split_index);
            let first_felt252 = first.into();
            let felt_to_append = first_felt252 * one_shift_left_bytes_felt252(self.pending_word_len)
                + self.pending_word.into();
            self.data.append(felt_to_append.try_into().unwrap());
            self.pending_word = second;
            self.pending_word_len = num_bytes - split_index;
            return;
        }

        self.append_word_inner(word, num_bytes);
    }

    fn append_word_inner(ref self: ByteArray, word: bytes31, num_bytes: u8) {
        if self.pending_word_len == 0 {
            if num_bytes == BYTES_IN_BYTES31 {
                self.data.append(word);
                return;
            } else {
                self.pending_word = word;
                self.pending_word_len = num_bytes;
                return;
            }
        }

        let word_felt252 = word.into();
        let pending_felt252 = self.pending_word.into();

        let num_total_pending_bytes = self.pending_word_len + num_bytes;
        // TODO(yg): either:
        // 1. remove this, change check in append_word to >= and leave `if index == len` in split_bytes31. Or:
        // 2. leave this and the check in append_word, remove the `if index == len` in split_bytes31, and change doc of split_bytes31 to assume index < len (not <=).
        if num_total_pending_bytes == BYTES_IN_BYTES31 {
            let felt_to_append = word_felt252 * one_shift_left_bytes_felt252(self.pending_word_len)
                + pending_felt252;

            self.data.append(felt_to_append.try_into().unwrap());
            self.pending_word = bytes31_const::<0>();
            self.pending_word_len = 0;
            return;
        }

        // num_total_pending_bytes < BYTES_IN_BYTES31
        let felt_to_append = word_felt252 * one_shift_left_bytes_felt252(self.pending_word_len)
            + pending_felt252;

        self.pending_word = felt_to_append.try_into().unwrap();
        self.pending_word_len = num_total_pending_bytes;
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

        let num_total_pending_bytes = self.pending_word_len + *other.pending_word_len;

        // split_index in [1, 30].
        let split_index = BYTES_IN_BYTES31 - self.pending_word_len;
        if split_index == BYTES_IN_U128 {
            loop {
                match other_data.pop_front() {
                    Option::Some(current_word) => {
                        self.append_split_index_medium(*current_word);
                    },
                    Option::None(_) => {
                        break;
                    }
                };
            };

            if num_total_pending_bytes > BYTES_IN_BYTES31 {
                self.append_split_index_medium(*other.pending_word);
                self.pending_word_len = num_total_pending_bytes - BYTES_IN_BYTES31;
            } else {
                self.append_word_inner(*other.pending_word, *other.pending_word_len);
            }
            return;
        }

        if split_index < BYTES_IN_U128 {
            loop {
                match other_data.pop_front() {
                    Option::Some(current_word) => {
                        self.append_split_index_short(*current_word, split_index);
                    },
                    Option::None(_) => {
                        break;
                    }
                };
            };

            if num_total_pending_bytes > BYTES_IN_BYTES31 {
                self.append_split_index_short(*other.pending_word, split_index);
                self.pending_word_len = num_total_pending_bytes - BYTES_IN_BYTES31;
            } else {
                self.append_word_inner(*other.pending_word, *other.pending_word_len);
            }
            return;
        }

        // split_index > BYTES_IN_U128
        loop {
            match other_data.pop_front() {
                Option::Some(current_word) => {
                    self.append_split_index_long(*current_word, split_index);
                },
                Option::None(_) => {
                    break;
                }
            };
        };

        if num_total_pending_bytes > BYTES_IN_BYTES31 {
            self.append_split_index_long(*other.pending_word, split_index);
            self.pending_word_len = num_total_pending_bytes - BYTES_IN_BYTES31;
        } else {
            self.append_word_inner(*other.pending_word, *other.pending_word_len);
        }
    }

    // A helper function to append a remainder to self, by:
    // 1. completing self.pending_word to a full word using `complete_full_word`, assuming its
    //    length as `bytes31` is `BYTES_IN_BYTES31 - self.pending_word_len`
    // 2. Setting self.pending_word to `new_pending`.
    fn append_split(ref self: ByteArray, complete_full_word: felt252, new_pending: bytes31) {
        let felt_to_append = complete_full_word
            * one_shift_left_bytes_felt252(self.pending_word_len)
            + self.pending_word.into();
        self.data.append(felt_to_append.try_into().unwrap());
        self.pending_word = new_pending;
    }

    // Concatenates 2 byte arrays and returns the result.
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

        let new_pending = (self.pending_word.into()
            + u8_to_felt252(byte) * one_shift_left_bytes_felt252(self.pending_word_len))
            .try_into()
            .unwrap();

        if self.pending_word_len != BYTES_IN_BYTES31 - 1 {
            self.pending_word = new_pending;
            self.pending_word_len += 1;
            return;
        }

        // self.pending_word_len == 30
        self.data.append(new_pending);
        self.pending_word = bytes31_const::<0>();
        self.pending_word_len = 0;
    }

    // Appends a single word to the end of `self`, given that the index of splitting `word` is < 16.
    // Note: this function doesn't update the new pending length of self. It's the caller's
    // responsibility.
    fn append_split_index_short(ref self: ByteArray, word: bytes31, split_index: u8) {
        let u256{low, high } = u256_from_felt252(word.into());

        let (low_quotient, low_remainder) = u128_safe_divmod(
            low, one_shift_left_bytes_u128(split_index).try_into().unwrap()
        );
        let right = u128_to_felt252(high)
            * one_shift_left_bytes_felt252(BYTES_IN_U128 - split_index)
            + u128_to_felt252(low_quotient);

        self.append_split(low_remainder.into(), right.try_into().unwrap());
    }

    // Appends a single word to the end of `self`, given that the index of splitting `word` is
    // exactly 16.
    // Note: this function doesn't update the new pending length of self. It's the caller's
    // responsibility.
    fn append_split_index_medium(ref self: ByteArray, word: bytes31) {
        let u256{low, high } = u256_from_felt252(word.into());
        self.append_split(low.into(), high.into());
    }

    // Appends a single word to the end of `self`, given that the index of splitting `word` is > 16.
    // Note: this function doesn't update the new pending length of self. It's the caller's
    // responsibility.
    fn append_split_index_long(ref self: ByteArray, word: bytes31, split_index: u8) {
        let u256{low, high } = u256_from_felt252(word.into());

        let (high_quotient, high_remainder) = u128_safe_divmod(
            high, one_shift_left_bytes_u128(split_index - BYTES_IN_U128).try_into().unwrap()
        );
        let left = u128_to_felt252(high_remainder) * POW_2_128 + u128_to_felt252(low);

        self.append_split(left, high_quotient.into());
    }
}
// TODO(yg): this is a workaround, use self.append_split() instead... and remove.
// fn append_split_workaround(
//     ref data: Array<bytes31>,
//     ref pending_word: bytes31,
//     pending_word_len: u8,
//     complete_full_word: felt252,
//     new_pending: bytes31
// ) {
//     let felt_to_append = complete_full_word * one_shift_left_bytes_felt252(pending_word_len)
//         + pending_word.into();
//     data.append(felt_to_append.try_into().unwrap());
//     pending_word = new_pending;
// }


