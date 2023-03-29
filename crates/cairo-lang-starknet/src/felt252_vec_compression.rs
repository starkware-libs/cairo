use cairo_lang_utils::bigint::BigUintAsHex;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use num_bigint::BigUint;
use num_traits::ToPrimitive;

/// Compresses a vector of `BigUintAsHex` representing felts into `result`, by creating a code
/// mapping, and then compressing several original code words into the given felts.
pub fn compress<Result: Extend<BigUintAsHex>>(values: &[BigUintAsHex], result: &mut Result) {
    let mut code = OrderedHashMap::<&BigUint, usize>::default();
    for value in values {
        let idx = code.len();
        code.entry(&value.value).or_insert(idx);
    }
    result.extend([BigUintAsHex { value: BigUint::from(code.len()) }].into_iter());
    result.extend(code.keys().map(|value| BigUintAsHex { value: (*value).clone() }));
    result.extend([BigUintAsHex { value: BigUint::from(values.len()) }].into_iter());
    let (bits_per_word, words_per_felt) = encoding_info(code.len());
    for values in values.chunks(words_per_felt) {
        let mut packed_value = BigUint::from(0u64);
        for value in values.iter().rev() {
            packed_value <<= bits_per_word;
            packed_value += code[&value.value];
        }
        result.extend([BigUintAsHex { value: packed_value }].into_iter());
    }
}

/// Decompresses `packed_values` created using `compress` into `result`.
pub fn decompress<Result: Extend<BigUintAsHex>>(
    packed_values: &[BigUintAsHex],
    result: &mut Result,
) -> Option<()> {
    let (packed_values, code_size) = pop_usize(packed_values)?;
    if code_size >= packed_values.len() {
        return None;
    }
    let (code, packed_values) = packed_values.split_at(code_size);
    let (packed_values, mut remaining_unpacked_size) = pop_usize(packed_values)?;
    let (bits_per_word, words_per_felt) = encoding_info(code.len());
    let word_mask: usize = (1 << bits_per_word) - 1;
    for packed_value in packed_values {
        let curr_words = std::cmp::min(words_per_felt, remaining_unpacked_size);
        let mut v = packed_value.value.clone();
        for _ in 0..curr_words {
            let code_word = (v.clone() & BigUint::from(word_mask)).to_usize().unwrap();
            result.extend([BigUintAsHex { value: code[code_word].value.clone() }].into_iter());
            v >>= bits_per_word;
        }
        remaining_unpacked_size -= curr_words;
    }
    assert_eq!(remaining_unpacked_size, 0);
    Some(())
}

/// Pops a `usize` from the slice while making sure it is a valid `usize`.
fn pop_usize(values: &[BigUintAsHex]) -> Option<(&[BigUintAsHex], usize)> {
    let (size, values) = values.split_first()?;
    Some((values, size.value.to_usize()?))
}

/// Returns the next power of 2 of `n`.
fn next_pow_of_2(n: usize) -> usize {
    let mut i = 0;
    while (1 << i) < n {
        i += 1;
    }
    i
}

/// Given the size of the code book, returns the number of bits required to encode a code word, and
/// the number of code words that can be encoded in a felt.
fn encoding_info(code_size: usize) -> (usize, usize) {
    const FELT_BITS: usize = 251;
    let bits_per_word = next_pow_of_2(code_size);
    (bits_per_word, FELT_BITS / bits_per_word)
}
