#[cfg(not(feature = "std"))]
use core::hash::Hasher;

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

#[test]
fn test_map() {
    let mut source = UnorderedHashMap::<_, _>::default();
    source.insert(20, 1);
    source.insert(8, 3);
    source.insert(9, 5);
    source.insert(11, 5);
    let target = source.map(|x| x * 2);
    let mut expected_target = UnorderedHashMap::default();
    expected_target.insert(11, 10);
    expected_target.insert(20, 2);
    expected_target.insert(9, 10);
    expected_target.insert(8, 6);

    assert_eq!(target, expected_target);
}

#[test]
fn test_aggregate_by() {
    let mut source = UnorderedHashMap::<_, _>::default();
    source.insert(20, 1);
    source.insert(8, 3);
    source.insert(9, 5);
    source.insert(11, 5);
    let target = source.aggregate_by(|x| *x < 10, |x, y| x + y, &0);
    let mut expected_target = UnorderedHashMap::default();
    expected_target.insert(true, 8);
    expected_target.insert(false, 6);

    assert_eq!(target, expected_target);
}

#[test]
fn test_iter_sorted() {
    let mut unordered = UnorderedHashMap::<_, _>::default();
    unordered.insert(XY { x: 11, y: 0 }, 0);
    unordered.insert(XY { x: 11, y: 5 }, 0);
    unordered.insert(XY { x: 12, y: 2 }, 0);
    unordered.insert(XY { x: 10, y: 9 }, 0);
    unordered.insert(XY { x: 10, y: 8 }, 0);

    let mut last = (&XY { x: 0, y: 0 }, &0);
    for cur in unordered.iter_sorted() {
        assert!(cur.cmp(&last) == core::cmp::Ordering::Greater);
        last = cur;
    }
}

#[test]
fn test_into_iter_sorted() {
    let mut unordered = UnorderedHashMap::<_, _>::default();
    unordered.insert(XY { x: 11, y: 0 }, 0);
    unordered.insert(XY { x: 11, y: 5 }, 0);
    unordered.insert(XY { x: 12, y: 2 }, 0);
    unordered.insert(XY { x: 10, y: 9 }, 0);
    unordered.insert(XY { x: 10, y: 8 }, 0);

    let mut last = (XY { x: 0, y: 0 }, 0);
    for cur in unordered.into_iter_sorted() {
        assert!(cur.cmp(&last) == core::cmp::Ordering::Greater);
        last = cur;
    }
}

#[test]
fn test_iter_sorted_by_key() {
    let mut unordered = UnorderedHashMap::<_, _>::default();
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
    let mut unordered = UnorderedHashMap::<_, _>::default();
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
    let mut unordered = UnorderedHashMap::<_, _>::default();
    unordered.insert(XY { x: 11, y: 0 }, 4);
    unordered.insert(XY { x: 11, y: 5 }, 7);
    unordered.insert(XY { x: 12, y: 2 }, 5);
    unordered.insert(XY { x: 10, y: 9 }, 9);
    unordered.insert(XY { x: 10, y: 8 }, 3);

    let filtered = unordered.filter(|k, v| k.x != 11 && *v != 9);

    let mut expected = UnorderedHashMap::default();
    expected.insert(XY { x: 12, y: 2 }, 5);
    expected.insert(XY { x: 10, y: 8 }, 3);

    assert_eq!(filtered, expected);
}

#[cfg(feature = "std")]
#[test]
fn test_merge() {
    // Test merge with a closure that removes the element.
    let mut value = UnorderedHashMap::<_, _>::from_iter([(1, 11), (2, 12), (3, 13)]);
    value.merge(&UnorderedHashMap::<_, _>::from_iter([(1, 21), (2, 22)]), |e, _| {
        e.remove();
    });
    assert_eq!(value, UnorderedHashMap::from_iter([(3, 13)]));

    // Test merge with a closure that updates the element.
    let mut value = UnorderedHashMap::<_, _>::from_iter([(1, 11), (2, 12), (3, 13)]);
    value.merge(&UnorderedHashMap::<_, _>::from_iter([(1, 21), (2, 22)]), |e, _| {
        *e.into_mut() += 20;
    });
    assert_eq!(value, UnorderedHashMap::from_iter([(1, 31), (2, 32), (3, 13)]));

    // Test merge with a closure that overrides the elements and adds a new one.
    let mut value = UnorderedHashMap::<_, _>::from_iter([(1, 11), (2, 12), (3, 13)]);
    value.merge(&UnorderedHashMap::<_, _>::from_iter([(1, 21), (2, 22), (4, 24)]), |e, v| {
        *e.into_mut() = *v;
    });
    assert_eq!(value, UnorderedHashMap::from_iter([(1, 21), (2, 22), (3, 13), (4, 24)]));
}
