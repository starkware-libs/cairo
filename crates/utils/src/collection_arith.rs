#[cfg(test)]
#[path = "collection_arith_test.rs"]
mod test;

use std::hash::Hash;
use std::ops::{Add, Sub};

use crate::ordered_hash_map::OrderedHashMap;

pub trait HasZero {
    fn zero() -> Self;
}
impl HasZero for i64 {
    fn zero() -> Self {
        0
    }
}

pub fn add_maps<Key: Hash + Eq, Value: HasZero + Add<Output = Value> + Clone + Eq>(
    lhs: OrderedHashMap<Key, Value>,
    rhs: OrderedHashMap<Key, Value>,
) -> OrderedHashMap<Key, Value> {
    let mut res = lhs;
    for (key, rhs_val) in rhs {
        let lhs_val = res.get(&key).cloned().unwrap_or(Value::zero());
        let new_val = lhs_val + rhs_val;
        if new_val == Value::zero() {
            res.swap_remove(&key);
        } else {
            res.insert(key, new_val);
        }
    }
    res
}

pub fn sub_maps<Key: Hash + Eq, Value: HasZero + Sub<Output = Value> + Clone + Eq>(
    lhs: OrderedHashMap<Key, Value>,
    rhs: OrderedHashMap<Key, Value>,
) -> OrderedHashMap<Key, Value> {
    let mut res = lhs;
    for (key, rhs_val) in rhs {
        let lhs_val = res.get(&key).cloned().unwrap_or(Value::zero());
        let new_val = lhs_val - rhs_val;
        if new_val == Value::zero() {
            res.swap_remove(&key);
        } else {
            res.insert(key, new_val);
        }
    }
    res
}
