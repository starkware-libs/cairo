use num_bigint::BigUint;
use sha3::{Digest, Keccak256};

#[cfg(test)]
#[path = "keccak_test.rs"]
mod test;

/// A variant of eth-keccak that computes a value that fits in a Starknet field element.
pub fn starknet_keccak(data: &[u8]) -> BigUint {
    let mut hasher = Keccak256::new();
    hasher.update(data);
    let mut result = hasher.finalize();

    // Truncate result to 250 bits.
    result[0] &= 3;
    BigUint::from_bytes_be(&result)
}
