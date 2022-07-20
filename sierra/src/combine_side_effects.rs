use crate::graph::{ApChange::*, SideEffects};

fn combine_side_effects(mut acc: SideEffects, other: &SideEffects) -> SideEffects {
    acc.ap_change = match (&acc.ap_change, &other.ap_change) {
        (Known(v1), Known(v2)) if v1 == v2 => Known(*v1),
        _ => Unknown,
    };
    acc
}

#[test]
fn test_combine_side_effects() {
    let se_known = |change| SideEffects {
        ap_change: Known(change),
    };
    let se_unknown = SideEffects { ap_change: Unknown };
    assert_eq!(combine_side_effects(se_known(1), &se_known(1)), se_known(1));
    assert_eq!(combine_side_effects(se_known(2), &se_known(1)), se_unknown);
    assert_eq!(combine_side_effects(se_known(2), &se_unknown), se_unknown);
    assert_eq!(
        combine_side_effects(se_unknown.clone(), &se_known(1)),
        se_unknown
    );
}
