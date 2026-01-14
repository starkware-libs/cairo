use cairo_lang_utils::bigint::BigUintAsHex;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::require;
use num_bigint::BigUint;
use num_integer::Integer;
use num_traits::{ToPrimitive, Zero};
use starknet_types_core::felt::Felt as Felt252;

/// Compresses a vector of `BigUintAsHex` representing felts into `result`, by creating a code
/// mapping, and then compressing several original code words into the given felts.
pub fn compress(values: &[BigUintAsHex], result: &mut Vec<BigUintAsHex>) {
    let mut code = OrderedHashMap::<&BigUintAsHex, usize>::default();
    for value in values {
        let idx = code.len();
        code.entry(value).or_insert(idx);
    }
    // Limiting the number of possible encodings by working only on powers of 2, as well as only
    // starting at 256 (or 8 bits per code word).
    let padded_code_size = std::cmp::max(256, code.len()).next_power_of_two();
    result.extend([code.len(), padded_code_size - code.len()].map(BigUintAsHex::from));
    result.extend(code.keys().copied().cloned());
    result.push(values.len().into());
    let words_per_felt = words_per_felt(padded_code_size);
    for values in values.chunks(words_per_felt) {
        let mut packed_value = BigUint::zero();
        for value in values.iter().rev() {
            packed_value *= padded_code_size;
            packed_value += code[&value];
        }
        result.push(packed_value.into());
    }
}

/// Decompresses `packed_values` created using `compress` into `result`.
pub fn decompress(packed_values: &[BigUintAsHex]) -> Option<Vec<&BigUint>> {
    let (packed_values, code_size) = pop_usize(packed_values)?;
    require(code_size < packed_values.len())?;
    let (packed_values, padding_size) = pop_usize(packed_values)?;
    let (code, packed_values) = packed_values.split_at(code_size);
    let (packed_values, mut remaining_unpacked_size) = pop_usize(packed_values)?;
    let padded_code_size = code_size.checked_add(padding_size)?;
    let words_per_felt = words_per_felt(padded_code_size);
    require(remaining_unpacked_size <= packed_values.len() * words_per_felt)?;
    let mut result = Vec::with_capacity(remaining_unpacked_size);
    for packed_value in packed_values {
        let curr_words = std::cmp::min(words_per_felt, remaining_unpacked_size);
        let mut iter = packed_value.value.iter_u64_digits();
        // Since all values are felt252s, we can assume 4 `u64` limbs are enough.
        let mut v: [u64; 4] = std::array::from_fn(|_| iter.next().unwrap_or_default());
        assert_eq!(iter.next(), None, "Unexpected extra limbs.");
        for _ in 0..curr_words {
            // No allocation 4 limbs long division implementation.
            let divisor = padded_code_size as u128;
            let mut rem: usize = 0;
            for limb in v.iter_mut().rev() {
                let (q, r) = (*limb as u128 | ((rem as u128) << 64)).div_rem(&divisor);
                *limb = q as u64;
                rem = r as usize;
            }
            result.push(&code.get(rem)?.value);
        }
        remaining_unpacked_size -= curr_words;
    }
    if remaining_unpacked_size == 0 { Some(result) } else { None }
}

/// Pops a `usize` from the slice while making sure it is a valid `usize`.
fn pop_usize(values: &[BigUintAsHex]) -> Option<(&[BigUintAsHex], usize)> {
    let (size, values) = values.split_first()?;
    Some((values, size.value.to_usize()?))
}

/// Given the size of the code book, returns the number of code words that can be encoded in a felt.
fn words_per_felt(padded_code_size: usize) -> usize {
    let mut count = 0;
    let prime = Felt252::prime();
    let mut max_encoded = BigUint::from(padded_code_size);
    while max_encoded < prime {
        max_encoded *= padded_code_size;
        count += 1;
    }
    count
}
