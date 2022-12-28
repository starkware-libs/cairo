#[cfg(test)]
#[path = "collection_arithmetics_test.rs"]
mod test;

use std::hash::Hash;
use std::ops::{Add, Sub};

use crate::ordered_hash_map::OrderedHashMap;

/// A trait for types which have a zero value.
///
/// Functions may assume the following:
/// * `x = x + zero() = zero() + x`
pub trait HasZero {
    /// Returns the zero value for the type.
    fn zero() -> Self;
}
impl HasZero for i64 {
    fn zero() -> Self {
        0
    }
}

/// Returns a map which contains the sum of the values from the given two maps, for each key.
///
/// If the key is missing from one of them, it is treated as zero.
pub fn add_maps<Key: Hash + Eq, Value: HasZero + Add<Output = Value> + Clone + Eq>(
    lhs: OrderedHashMap<Key, Value>,
    rhs: OrderedHashMap<Key, Value>,
) -> OrderedHashMap<Key, Value> {
    let mut res = lhs;
    for (key, rhs_val) in rhs {
        let lhs_val = res.get(&key).cloned().unwrap_or_else(Value::zero);
        let new_val = lhs_val + rhs_val;
        if new_val == Value::zero() {
            res.swap_remove(&key);
        } else {
            res.insert(key, new_val);
        }
    }
    res
}

/// Returns a map which contains the difference of the values from the given two maps, for each key.
///
/// If the key is missing from one of them, it is treated as zero.
pub fn sub_maps<Key: Hash + Eq, Value: HasZero + Sub<Output = Value> + Clone + Eq>(
    lhs: OrderedHashMap<Key, Value>,
    rhs: OrderedHashMap<Key, Value>,
) -> OrderedHashMap<Key, Value> {
    let mut res = lhs;
    for (key, rhs_val) in rhs {
        let lhs_val = res.get(&key).cloned().unwrap_or_else(Value::zero);
        let new_val = lhs_val - rhs_val;
        if new_val == Value::zero() {
            res.swap_remove(&key);
        } else {
            res.insert(key, new_val);
        }
    }
    res
}
