#[cfg(feature = "std")]
use std::collections::hash_map::RandomState as HashBuilderType;

#[cfg(not(feature = "std"))]
use hashbrown::DefaultHashBuilder as HashBuilderType;

use crate::collection_arithmetics::{AddCollection, SubCollection};
use crate::ordered_hash_map::OrderedHashMap;

#[test]
fn test_add_map_and_sub_map() {
    crate::logging::init_logging(crate::logging::level::ERROR);
    let x = OrderedHashMap::<i64, i64, HashBuilderType>::from_iter([
        (10, 3),
        (20, 7),
        (30, 3),
        (40, 3),
    ]);
    let y = OrderedHashMap::<i64, i64, HashBuilderType>::from_iter([
        (0, 2),
        (10, 5),
        (30, -3),
        (40, 3),
    ]);

    assert_eq!(
        x.clone().add_collection(y.iter().map(|(k, v)| (*k, *v))),
        OrderedHashMap::<i64, i64, HashBuilderType>::from_iter([(10, 8), (20, 7), (0, 2), (40, 6)])
    );
    assert_eq!(
        x.sub_collection(y),
        OrderedHashMap::<i64, i64, HashBuilderType>::from_iter([
            (10, -2),
            (20, 7),
            (30, 6),
            (0, -2)
        ])
    );
}
