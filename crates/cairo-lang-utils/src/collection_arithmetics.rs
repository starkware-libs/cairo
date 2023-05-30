#[cfg(test)]
#[path = "collection_arithmetics_test.rs"]
mod test;

use std::hash::Hash;
use std::ops::{Add, Sub};

use indexmap::map::Entry;

use crate::ordered_hash_map::OrderedHashMap;

/// A trait for types which have a zero value.
///
/// Functions may assume the following:
/// * `x = x + zero() = zero() + x`
pub trait HasZero {
    /// Returns the zero value for the type.
    fn zero() -> Self;
}
impl HasZero for i32 {
    fn zero() -> Self {
        0
    }
}
impl HasZero for i64 {
    fn zero() -> Self {
        0
    }
}

/// Returns a map which contains the sum of the values from the given two maps, for each key.
///
/// If the key is missing from one of them, it is treated as zero.
pub fn add_maps<
    Key: Hash + Eq,
    Value: HasZero + Add<Output = Value> + Clone + Eq,
    Rhs: IntoIterator<Item = (Key, Value)>,
>(
    lhs: OrderedHashMap<Key, Value>,
    rhs: Rhs,
) -> OrderedHashMap<Key, Value> {
    merge_maps(lhs, rhs, |a, b| a + b)
}

/// Returns a map which contains the difference of the values from the given two maps, for each key.
///
/// If the key is missing from one of them, it is treated as zero.
pub fn sub_maps<
    Key: Hash + Eq,
    Value: HasZero + Sub<Output = Value> + Clone + Eq,
    Rhs: IntoIterator<Item = (Key, Value)>,
>(
    lhs: OrderedHashMap<Key, Value>,
    rhs: Rhs,
) -> OrderedHashMap<Key, Value> {
    merge_maps(lhs, rhs, |a, b| a - b)
}

/// Returns a map which contains the combination by using `action` of the values from the given two
/// maps, for each key.
///
/// If the key is missing from one of them, it is treated as zero.
fn merge_maps<
    Key: Hash + Eq,
    Value: HasZero + Clone + Eq,
    Rhs: IntoIterator<Item = (Key, Value)>,
    Action: Fn(Value, Value) -> Value,
>(
    lhs: OrderedHashMap<Key, Value>,
    rhs: Rhs,
    action: Action,
) -> OrderedHashMap<Key, Value> {
    let mut res = lhs;
    for (key, rhs_val) in rhs {
        match res.entry(key) {
            Entry::Occupied(mut e) => {
                let new_val = action(e.get().clone(), rhs_val);
                if new_val == Value::zero() {
                    e.swap_remove();
                } else {
                    e.insert(new_val);
                }
            }
            Entry::Vacant(e) => {
                if rhs_val != Value::zero() {
                    e.insert(action(Value::zero(), rhs_val));
                }
            }
        }
    }
    res
}
