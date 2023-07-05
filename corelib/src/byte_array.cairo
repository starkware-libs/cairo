use traits::{Into, TryInto};
use bytes_31::{U128IntoBytes31, U8IntoBytes31};
use array::{ArrayTrait, SpanTrait};
use option::OptionTrait;
use integer::{u128_safe_divmod, u128_to_felt252, u8_to_felt252, u256_from_felt252};
use bytes_31::{
    BYTES_IN_BYTES31, bytes31_try_from_felt252, one_shift_left_bytes_felt252,
    one_shift_left_bytes_u128, Bytes31Trait
};
use zeroable::NonZeroIntoImpl;

// TODO(yuval): don't allow creation of invalid ByteArray?
#[derive(Drop)]
struct ByteArray {
    // Full "words" of 31 bytes each.
    data: Array<bytes31>,
    // Less than 31 bytes. The number of bytes in here is specified in `pending_word_len`.
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

#[generate_trait]
impl ByteArrayImpl of ByteArrayTrait {
    // TODO(yuval): add a `new` function for initialization.

    // Appends a single word of `num_bytes` bytes to the end of the ByteArray.
    // Note: this function assumes `word` has no more than `num_bytes` bytes of data. If it has, it
    // can corrupt the ByteArray. Thus, this should be a private function. We can add masking but it
    // would be more expensive.
    fn append_word(ref self: ByteArray, word: bytes31, num_bytes: u8) {
        if (num_bytes == 0) {
            return;
        }
        assert(num_bytes <= BYTES_IN_BYTES31, 'num_bytes > 31');

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
        if num_total_pending_bytes < BYTES_IN_BYTES31 {
            let sum = word_felt252 * one_shift_left_bytes_felt252(self.pending_word_len)
                + pending_felt252;

            self.pending_word = sum.try_into().unwrap();
            self.pending_word_len = num_total_pending_bytes;
            return;
        }

        if num_total_pending_bytes == BYTES_IN_BYTES31 {
            let sum = (word_felt252 * one_shift_left_bytes_felt252(self.pending_word_len))
                + pending_felt252;

            self.data.append(sum.try_into().unwrap());
            self.pending_word = bytes31_const::<0>();
            self.pending_word_len = 0;
            return;
        }

        // num_total_pending_bytes > BYTES_IN_BYTES31
        let split_index = BYTES_IN_BYTES31 - self.pending_word_len;
        let (first, second) = word.split_bytes31(len: num_bytes, index: split_index);
        let first_felt252 = first.into();
        let felt_to_append = first_felt252 * one_shift_left_bytes_felt252(self.pending_word_len)
            + pending_felt252;
        self.data.append(felt_to_append.try_into().unwrap());
        self.pending_word = second;
        self.pending_word_len = num_bytes - split_index;
    }

    // Appends a byte array to the end of `self`.
    fn append(ref self: ByteArray, mut other: @ByteArray) {
        let mut other_data = other.data.span();
        loop {
            match other_data.pop_front() {
                Option::Some(current_word) => {
                    self.append_word(*current_word, BYTES_IN_BYTES31);
                },
                Option::None(_) => {
                    break;
                }
            };
        };
        self.append_word(*other.pending_word, *other.pending_word_len);
    }

    // Concatenates 2 byte arrays and returns the result.
    fn concat(left: @ByteArray, right: @ByteArray) -> ByteArray {
        let mut result: ByteArray = Default::default();
        result.append(left);
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
}
