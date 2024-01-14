#[cfg(not(feature = "std"))]
use core::hash::Hasher;

use super::UnorderedHashMap;
#[cfg(feature = "std")]
use crate::ordered_hash_map::OrderedHashMap;

#[cfg(not(feature = "std"))]
#[derive(Default)]
struct MyHasher;

#[cfg(not(feature = "std"))]
impl Hasher for MyHasher {
    fn write(&mut self, _bytes: &[u8]) {}
    fn finish(&self) -> u64 {
        0
    }
}

#[cfg(not(feature = "std"))]
#[test]
fn test_no_std_aggregate_by() {
    let hasher = hashbrown::hash_map::DefaultHashBuilder::default();
    let mut source = UnorderedHashMap::with_hasher(hasher.clone());
    source.insert(20, 1);
    source.insert(8, 3);
    source.insert(9, 5);
    source.insert(11, 5);
    let target = source.aggregate_by(|x| *x < 10, |x, y| x + y, &0, hasher.clone());
    let mut expected_target = UnorderedHashMap::with_hasher(hasher);
    expected_target.insert(true, 8);
    expected_target.insert(false, 6);

    if target != expected_target {
        panic!("target != expected_target");
    }
}

#[cfg(feature = "std")]
#[test]
fn test_aggregate_by() {
    let mut source = UnorderedHashMap::new();
    source.insert(20, 1);
    source.insert(8, 3);
    source.insert(9, 5);
    source.insert(11, 5);
    let target = source.aggregate_by(|x| *x < 10, |x, y| x + y, &0);
    let mut expected_target = UnorderedHashMap::new();
    expected_target.insert(true, 8);
    expected_target.insert(false, 6);

    assert_eq!(target, expected_target);
}

#[cfg(feature = "std")]
#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Debug, Clone)]
struct XY {
    x: u32,
    y: u32,
}

#[cfg(feature = "std")]
#[test]
fn test_iter_sorted() {
    let mut unordered1 = UnorderedHashMap::new();
    unordered1.insert(XY { x: 11, y: 0 }, 0);
    unordered1.insert(XY { x: 11, y: 5 }, 0);
    unordered1.insert(XY { x: 12, y: 2 }, 0);
    unordered1.insert(XY { x: 10, y: 9 }, 0);
    unordered1.insert(XY { x: 10, y: 8 }, 0);

    let mut unordered2 = UnorderedHashMap::new();
    unordered2.insert(XY { x: 12, y: 2 }, 0);
    unordered2.insert(XY { x: 10, y: 9 }, 0);
    unordered2.insert(XY { x: 11, y: 5 }, 0);
    unordered2.insert(XY { x: 10, y: 8 }, 0);
    unordered2.insert(XY { x: 11, y: 0 }, 0);

    let ordered1: OrderedHashMap<_, _> = unordered1.iter_sorted().collect();
    let ordered2: OrderedHashMap<_, _> = unordered2.iter_sorted().collect();

    assert_eq!(ordered1, ordered2);
}

#[cfg(feature = "std")]
#[test]
fn test_into_iter_sorted() {
    let mut unordered1 = UnorderedHashMap::new();
    unordered1.insert(XY { x: 11, y: 0 }, 0);
    unordered1.insert(XY { x: 11, y: 5 }, 0);
    unordered1.insert(XY { x: 12, y: 2 }, 0);
    unordered1.insert(XY { x: 10, y: 9 }, 0);
    unordered1.insert(XY { x: 10, y: 8 }, 0);

    let mut unordered2 = UnorderedHashMap::new();
    unordered2.insert(XY { x: 12, y: 2 }, 0);
    unordered2.insert(XY { x: 10, y: 9 }, 0);
    unordered2.insert(XY { x: 11, y: 5 }, 0);
    unordered2.insert(XY { x: 10, y: 8 }, 0);
    unordered2.insert(XY { x: 11, y: 0 }, 0);

    let ordered1: OrderedHashMap<_, _> = unordered1.into_iter_sorted().collect();
    let ordered2: OrderedHashMap<_, _> = unordered2.into_iter_sorted().collect();

    assert_eq!(ordered1, ordered2);
}
