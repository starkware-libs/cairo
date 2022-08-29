use super::{take_cost, CostBag, CostBagError, CostBagUnion};
use crate::cost_bag::merge_cost;

#[test]
fn test_take_cost() {
    assert_eq!(
        take_cost(CostBagUnion::new(), &"id".into()),
        Err(CostBagError::OutOfResource("id".into()))
    );
    assert_eq!(
        take_cost(CostBagUnion::from([CostBag::from([])]), &"id".into()),
        Err(CostBagError::OutOfResource("id".into()))
    );
    assert_eq!(
        take_cost(
            CostBagUnion::from([
                CostBag::from([]),                                      // Removed.
                CostBag::from([("id".into(), 1)]),                      // Becomes empty.
                CostBag::from([("other".into(), 1)]),                   // Removed.
                CostBag::from([("id".into(), 1), ("other".into(), 1)]), // Only other remains.
                CostBag::from([("id".into(), 3)]),                      // Reduced 3 to 2.
            ]),
            &"id".into()
        ),
        Ok(CostBagUnion::from([
            CostBag::new(),
            CostBag::from([("id".into(), 2)]),
            CostBag::from([("other".into(), 1)])
        ]))
    );
}

#[test]
fn test_merge_cost() {
    assert_eq!(merge_cost(&CostBagUnion::new(), &CostBagUnion::new()), CostBagUnion::new());
    assert_eq!(
        merge_cost(&CostBagUnion::new(), &CostBagUnion::from([CostBag::from([])])),
        CostBagUnion::new()
    );
    assert_eq!(
        merge_cost(
            &CostBagUnion::from([CostBag::from([])]),
            &CostBagUnion::from([CostBag::from([])])
        ),
        CostBagUnion::from([CostBag::from([])])
    );
    assert_eq!(
        merge_cost(
            &CostBagUnion::from([
                CostBag::from([("id1".into(), 1), ("id2".into(), 1)]),
                CostBag::from([("id2".into(), 1), ("id3".into(), 2)])
            ]),
            &CostBagUnion::from([
                CostBag::from([("id1".into(), 2), ("id3".into(), 2)]),
                CostBag::from([("id3".into(), 1), ("id4".into(), 1)])
            ])
        ),
        CostBagUnion::from([
            CostBag::new(),
            CostBag::from([("id1".into(), 1)]),
            CostBag::from([("id3".into(), 1)]),
            CostBag::from([("id3".into(), 2)])
        ])
    );
}
