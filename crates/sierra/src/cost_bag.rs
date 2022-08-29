use std::cmp::min;
use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, HashSet};

use itertools::{chain, iproduct};
use thiserror::Error;

use crate::ids::ConcreteLibFuncId;

#[cfg(test)]
#[path = "cost_bag_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum CostBagError {
    #[error("Could not take the cost out")]
    OutOfResource(ConcreteLibFuncId),
}

pub type CostBag = BTreeMap<ConcreteLibFuncId, usize>;
pub type CostBagUnion = HashSet<CostBag>;

/// Take a cost from a cost bag - fail if there's no available cost.
fn inner_take_cost(mut bag: CostBag, id: &ConcreteLibFuncId) -> Result<CostBag, CostBagError> {
    match bag.entry(id.clone()) {
        Entry::Occupied(mut e) => {
            *e.get_mut() -= 1;
            if *e.get() == 0 {
                e.remove();
            }
            Ok(())
        }
        Entry::Vacant(_) => Err(CostBagError::OutOfResource(id.clone())),
    }?;
    Ok(bag)
}

/// Take a cost from a union of cost bags - cost bags that did not fit would be dropped, fail if
/// there's no option in the union with available cost.
pub fn take_cost(bag: CostBagUnion, id: &ConcreteLibFuncId) -> Result<CostBagUnion, CostBagError> {
    let bag = CostBagUnion::from_iter(
        bag.into_iter().filter_map(|inner| inner_take_cost(inner, id).ok()),
    );
    if bag.is_empty() { Err(CostBagError::OutOfResource(id.clone())) } else { Ok(bag) }
}

/// Merges two cost bags by leaving the minimum of every cost.
fn inner_merge_costs(bag1: &CostBag, bag2: &CostBag) -> CostBag {
    CostBag::from_iter(chain!(
        bag1.iter()
            .filter_map(|(id, count)| bag2.get(id).map(|other| (id.clone(), *min(count, other)))),
        bag2.iter()
            .filter_map(|(id, count)| bag1.get(id).map(|other| (id.clone(), *min(count, other)))),
    ))
}

/// Merges two cost unions by returning the union of all internal merges.
pub fn merge_cost(bag1: &CostBagUnion, bag2: &CostBagUnion) -> CostBagUnion {
    CostBagUnion::from_iter(
        iproduct!(bag1, bag2).map(|(inner1, inner2)| inner_merge_costs(inner1, inner2)),
    )
}
