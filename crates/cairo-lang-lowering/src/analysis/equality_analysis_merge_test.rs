use super::{
    ArrayItems, EqualityState, FieldVar, Placeholder, fresh_placeholder, merge_array_items,
};

/// Builds tracked array contents with `suffix_len` distinct placeholder elements.
fn items(
    prefix_placeholder: Option<Placeholder>,
    suffix_len: usize,
    next_placeholder: &mut usize,
) -> ArrayItems {
    ArrayItems {
        prefix_placeholder,
        suffix: (0..suffix_len)
            .map(|_| FieldVar::Placeholder(fresh_placeholder(next_placeholder)))
            .collect(),
    }
}

/// Runs [`merge_array_items`] on a fresh state.
fn merge(
    items1: &ArrayItems,
    items2: &ArrayItems,
    next_placeholder: &mut usize,
) -> (ArrayItems, bool) {
    merge_array_items(items1, items2, &mut EqualityState::default(), next_placeholder)
}

#[test]
fn fully_known_same_length_meets_in_place() {
    let next = &mut 0;
    let (items1, items2) = (items(None, 3, next), items(None, 3, next));
    let (met, _) = merge(&items1, &items2, next);
    assert_eq!(met.prefix_placeholder, None);
    assert_eq!(met.suffix.len(), 3);
}

#[test]
fn fully_known_different_lengths_keep_all_common_elements() {
    let next = &mut 0;
    // [a, b] vs [c, d, e]: the merged prefix covers zero elements of the shorter side and one
    // of the longer, and both common trailing elements survive.
    for (items1, items2) in
        [(items(None, 2, next), items(None, 3, next)), (items(None, 3, next), items(None, 2, next))]
    {
        let (met, _) = merge(&items1, &items2, next);
        assert!(met.prefix_placeholder.is_some());
        assert_eq!(met.suffix.len(), 2);
    }
}

#[test]
fn shared_prefix_and_shape_keep_the_prefix() {
    let next = &mut 0;
    let prefix = fresh_placeholder(next);
    let (items1, items2) = (items(Some(prefix), 3, next), items(Some(prefix), 3, next));
    let (met, any_data) = merge(&items1, &items2, next);
    assert_eq!(met.prefix_placeholder, Some(prefix));
    assert_eq!(met.suffix.len(), 3);
    assert!(any_data);
}

#[test]
fn distinct_prefixes_degrade_to_a_fresh_prefix() {
    let next = &mut 0;
    let (items1, items2) = (
        items(Some(fresh_placeholder(next)), 3, next),
        items(Some(fresh_placeholder(next)), 3, next),
    );
    let (met, _) = merge(&items1, &items2, next);
    let met_prefix = met.prefix_placeholder.unwrap();
    assert_ne!(Some(met_prefix), items1.prefix_placeholder);
    assert_ne!(Some(met_prefix), items2.prefix_placeholder);
    assert_eq!(met.suffix.len(), 3);
}

#[test]
fn shared_prefix_with_different_lengths_degrades() {
    let next = &mut 0;
    // The longer side's extra element joins its side of the merged prefix, so the shared
    // identity is lost, but all common trailing elements survive.
    let prefix = fresh_placeholder(next);
    let (items1, items2) = (items(Some(prefix), 2, next), items(Some(prefix), 3, next));
    let (met, _) = merge(&items1, &items2, next);
    assert_ne!(met.prefix_placeholder, Some(prefix));
    assert!(met.prefix_placeholder.is_some());
    assert_eq!(met.suffix.len(), 2);
}

#[test]
fn mixed_prefix_presence_keeps_all_common_elements() {
    let next = &mut 0;
    // A fully-known side meeting a prefixed side: the merged prefix covers zero elements on
    // the fully-known side, so nothing is dropped — in either argument order.
    for (items1, items2) in [
        (items(None, 3, next), items(Some(fresh_placeholder(next)), 3, next)),
        (items(Some(fresh_placeholder(next)), 3, next), items(None, 3, next)),
    ] {
        let (met, _) = merge(&items1, &items2, next);
        assert!(met.prefix_placeholder.is_some());
        assert_eq!(met.suffix.len(), 3);
    }
}

#[test]
fn known_empty_side_merges_to_a_prefix_only_relation() {
    let next = &mut 0;
    // [] vs [prefix]: the met contents are a fresh, possibly-empty prefix with no tracked
    // elements — sound for both sides.
    let (items1, items2) = (items(None, 0, next), items(Some(fresh_placeholder(next)), 0, next));
    let (met, any_data) = merge(&items1, &items2, next);
    assert!(met.prefix_placeholder.is_some());
    assert!(met.suffix.is_empty());
    assert!(!any_data);
}
