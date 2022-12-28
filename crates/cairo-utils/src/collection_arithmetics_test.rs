use test_log::test;

use crate::collection_arithmetics::{add_maps, sub_maps};
use crate::ordered_hash_map::OrderedHashMap;

#[test]
fn test_add_map_and_sub_map() {
    let x = OrderedHashMap::<i64, i64>::from_iter([(10, 3), (20, 7), (30, 3), (40, 3)]);
    let y = OrderedHashMap::<i64, i64>::from_iter([(0, 2), (10, 5), (30, -3), (40, 3)]);

    assert_eq!(
        add_maps(x.clone(), y.clone()),
        OrderedHashMap::<i64, i64>::from_iter([(10, 8), (20, 7), (0, 2), (40, 6)])
    );
    assert_eq!(
        sub_maps(x, y),
        OrderedHashMap::<i64, i64>::from_iter([(10, -2), (20, 7), (30, 6), (0, -2)])
    );
}
