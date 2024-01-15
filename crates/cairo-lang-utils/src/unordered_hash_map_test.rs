use super::UnorderedHashMap;

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

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Debug, Clone)]
struct XY {
    x: u32,
    y: u32,
}

/// A utility function for this test module to create an empty hashmap based on the configuration
/// (std/no-std)
#[cfg(not(feature = "std"))]
fn new_hashmap<K, V>() -> UnorderedHashMap<K, V, hashbrown::hash_map::DefaultHashBuilder> {
    let hasher = hashbrown::hash_map::DefaultHashBuilder::default();
    UnorderedHashMap::with_hasher(hasher)
}
#[cfg(feature = "std")]
fn new_hashmap<K, V>() -> UnorderedHashMap<K, V> {
    UnorderedHashMap::new()
}

#[test]
fn test_aggregate_by() {
    let mut source = new_hashmap();
    source.insert(20, 1);
    source.insert(8, 3);
    source.insert(9, 5);
    source.insert(11, 5);
    let target = source.aggregate_by(|x| *x < 10, |x, y| x + y, &0);
    let mut expected_target = new_hashmap();
    expected_target.insert(true, 8);
    expected_target.insert(false, 6);

    assert_eq!(target, expected_target);
}

#[test]
fn test_iter_sorted() {
    let mut unordered = new_hashmap();
    unordered.insert(XY { x: 11, y: 0 }, 0);
    unordered.insert(XY { x: 11, y: 5 }, 0);
    unordered.insert(XY { x: 12, y: 2 }, 0);
    unordered.insert(XY { x: 10, y: 9 }, 0);
    unordered.insert(XY { x: 10, y: 8 }, 0);

    let mut last = 0;
    for (key, val) in unordered.iter_sorted_by_key(|a| a.0.x + a.0.y + a.1) {
        let cur = key.x + key.y + val;
        assert!(cur.cmp(&last) == core::cmp::Ordering::Greater);
        last = cur;
    }
}

#[test]
fn test_into_iter_sorted() {
    let mut unordered = new_hashmap();
    unordered.insert(XY { x: 11, y: 0 }, 0);
    unordered.insert(XY { x: 11, y: 5 }, 0);
    unordered.insert(XY { x: 12, y: 2 }, 0);
    unordered.insert(XY { x: 10, y: 9 }, 0);
    unordered.insert(XY { x: 10, y: 8 }, 0);

    let mut last = 0;
    for (key, val) in unordered.into_iter_sorted_by_key(|a| a.0.x + a.0.y + a.1) {
        let cur = key.x + key.y + val;
        assert!(cur.cmp(&last) == core::cmp::Ordering::Greater);
        last = cur;
    }
}

#[test]
fn test_iter_sorted_by_key() {
    let mut unordered = new_hashmap();
    unordered.insert(XY { x: 11, y: 0 }, 4); // sum = 15
    unordered.insert(XY { x: 11, y: 5 }, 7); // sum = 23
    unordered.insert(XY { x: 12, y: 2 }, 5); // sum = 19
    unordered.insert(XY { x: 10, y: 9 }, 9); // sum = 28
    unordered.insert(XY { x: 10, y: 8 }, 3); // sum = 21

    let mut last_sum = 0;
    for (key, val) in unordered.iter_sorted_by_key(|a| a.0.x + a.0.y + a.1) {
        let cur_sum = key.x + key.y + val;
        assert!(cur_sum >= last_sum);
        last_sum = cur_sum;
    }
}

#[test]
fn test_into_iter_sorted_by_key() {
    let mut unordered = new_hashmap();
    unordered.insert(XY { x: 11, y: 0 }, 4); // sum = 15
    unordered.insert(XY { x: 11, y: 5 }, 7); // sum = 23
    unordered.insert(XY { x: 12, y: 2 }, 5); // sum = 19
    unordered.insert(XY { x: 10, y: 9 }, 9); // sum = 28
    unordered.insert(XY { x: 10, y: 8 }, 3); // sum = 21

    let mut last_sum = 0;
    for (key, val) in unordered.into_iter_sorted_by_key(|a| a.0.x + a.0.y + a.1) {
        let cur_sum = key.x + key.y + val;
        assert!(cur_sum >= last_sum);
        last_sum = cur_sum;
    }
}

#[test]
fn test_filter() {
    let mut unordered = new_hashmap();
    unordered.insert(XY { x: 11, y: 0 }, 4);
    unordered.insert(XY { x: 11, y: 5 }, 7);
    unordered.insert(XY { x: 12, y: 2 }, 5);
    unordered.insert(XY { x: 10, y: 9 }, 9);
    unordered.insert(XY { x: 10, y: 8 }, 3);

    let filtered = unordered.filter(|k, v| k.x != 11 && *v != 9);

    let mut expected = new_hashmap();
    expected.insert(XY { x: 12, y: 2 }, 5);
    expected.insert(XY { x: 10, y: 8 }, 3);

    assert_eq!(filtered, expected);
}
