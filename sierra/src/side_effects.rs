use std::cmp::max;

use crate::context::{Context, ResourceMap};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SideEffects {
    pub ap_change: Option<usize>,
    pub local_writes: usize,
    pub resource_usages: ResourceMap,
}

impl SideEffects {
    pub(crate) fn new(before: &Context, after: &Context) -> SideEffects {
        SideEffects {
            ap_change: if after.temp_invalidated {
                None
            } else {
                Some(after.temp_cursur - before.temp_cursur)
            },
            local_writes: after.local_cursur - before.local_cursur,
            resource_usages: chain!(
                before
                    .resources
                    .iter()
                    .map(|(r, count)| (r.clone(), count - after.resources.get(r).unwrap_or(&0),)),
                after
                    .resources
                    .iter()
                    .map(|(r, count)| (r.clone(), before.resources.get(r).unwrap_or(&0) - count))
            )
            .collect(),
        }
    }

    pub(crate) fn add(self: &Self, other: &SideEffects) -> SideEffects {
        SideEffects {
            ap_change: match (&self.ap_change, &other.ap_change) {
                (Some(c1), Some(c2)) => Some(c1 + c2),
                _ => None,
            },
            local_writes: self.local_writes + other.local_writes,
            resource_usages: chain!(
                self.resource_usages.iter().map(|(r, change)| (
                    r.clone(),
                    change + other.resource_usages.get(r).unwrap_or(&0),
                )),
                other.resource_usages.iter().map(|(r, change)| (
                    r.clone(),
                    self.resource_usages.get(r).unwrap_or(&0) + change
                ))
            )
            .collect(),
        }
    }

    pub(crate) fn converge(self: &Self, other: &SideEffects) -> SideEffects {
        SideEffects {
            ap_change: match (&self.ap_change, &other.ap_change) {
                (Some(c1), Some(c2)) if c1 == c2 => Some(*c1),
                _ => None,
            },
            local_writes: max(self.local_writes, other.local_writes),
            resource_usages: chain!(
                self.resource_usages.iter().map(|(r, change)| (
                    r.clone(),
                    *max(change, other.resource_usages.get(r).unwrap_or(&0))
                )),
                other.resource_usages.iter().map(|(r, change)| (
                    r.clone(),
                    *max(self.resource_usages.get(r).unwrap_or(&0), change)
                ))
            )
            .collect(),
        }
    }
}

#[cfg(test)]
mod function {
    use super::*;
    use crate::graph::Identifier;
    #[test]
    fn test_new() {
        let g1 = Identifier("gas1".to_string());
        let g2 = Identifier("gas2".to_string());
        let g3 = Identifier("gas3".to_string());
        assert_eq!(
            SideEffects::new(
                &Context {
                    local_cursur: 3,
                    local_allocated: false,
                    temp_used: true,
                    temp_cursur: 10,
                    temp_invalidated: false,
                    resources: ResourceMap::from([(g1.clone(), 1), (g3.clone(), 1)]),
                },
                &Context {
                    local_cursur: 7,
                    local_allocated: false,
                    temp_used: true,
                    temp_cursur: 21,
                    temp_invalidated: false,
                    resources: ResourceMap::from([(g2.clone(), 1), (g3.clone(), 1)]),
                }
            ),
            SideEffects {
                ap_change: Some(11),
                local_writes: 4,
                resource_usages: ResourceMap::from([
                    (g1.clone(), 1),
                    (g2.clone(), -1),
                    (g3.clone(), 0)
                ]),
            }
        );
        assert_eq!(
            SideEffects::new(
                &Context {
                    local_cursur: 7,
                    local_allocated: false,
                    temp_used: true,
                    temp_cursur: 10,
                    temp_invalidated: false,
                    resources: ResourceMap::new(),
                },
                &Context {
                    local_cursur: 8,
                    local_allocated: false,
                    temp_used: true,
                    temp_cursur: 21,
                    temp_invalidated: true,
                    resources: ResourceMap::new(),
                }
            ),
            SideEffects {
                ap_change: None,
                local_writes: 1,
                resource_usages: ResourceMap::new(),
            }
        );
    }

    #[test]
    fn test_add() {
        let g1 = Identifier("gas1".to_string());
        let g2 = Identifier("gas2".to_string());
        let g3 = Identifier("gas3".to_string());
        assert_eq!(
            SideEffects {
                ap_change: Some(2),
                local_writes: 1,
                resource_usages: ResourceMap::from([(g1.clone(), 1), (g2.clone(), 2)]),
            }
            .add(&SideEffects {
                ap_change: Some(5),
                local_writes: 7,
                resource_usages: ResourceMap::from([(g1.clone(), 1), (g3.clone(), 2)]),
            }),
            SideEffects {
                ap_change: Some(7),
                local_writes: 8,
                resource_usages: ResourceMap::from([(g1, 2), (g2, 2), (g3, 2)]),
            }
        );
        assert_eq!(
            SideEffects {
                ap_change: None,
                local_writes: 3,
                resource_usages: ResourceMap::new(),
            }
            .add(&SideEffects {
                ap_change: Some(5),
                local_writes: 1,
                resource_usages: ResourceMap::new(),
            }),
            SideEffects {
                ap_change: None,
                local_writes: 4,
                resource_usages: ResourceMap::new(),
            }
        );
        assert_eq!(
            SideEffects {
                ap_change: Some(2),
                local_writes: 9,
                resource_usages: ResourceMap::new(),
            }
            .add(&SideEffects {
                ap_change: None,
                local_writes: 0,
                resource_usages: ResourceMap::new(),
            }),
            SideEffects {
                ap_change: None,
                local_writes: 9,
                resource_usages: ResourceMap::new(),
            }
        );
    }

    #[test]
    fn test_converge() {
        let g1 = Identifier("gas1".to_string());
        let g2 = Identifier("gas2".to_string());
        let g3 = Identifier("gas3".to_string());
        assert_eq!(
            SideEffects {
                ap_change: Some(2),
                local_writes: 1,
                resource_usages: ResourceMap::from([(g1.clone(), 1), (g2.clone(), 2)]),
            }
            .converge(&SideEffects {
                ap_change: Some(2),
                local_writes: 7,
                resource_usages: ResourceMap::from([(g1.clone(), -1), (g3.clone(), 2)]),
            }),
            SideEffects {
                ap_change: Some(2),
                local_writes: 7,
                resource_usages: ResourceMap::from([(g1, 1), (g2, 2), (g3, 2)]),
            }
        );
        assert_eq!(
            SideEffects {
                ap_change: Some(2),
                local_writes: 9,
                resource_usages: ResourceMap::new(),
            }
            .converge(&SideEffects {
                ap_change: Some(3),
                local_writes: 7,
                resource_usages: ResourceMap::new(),
            }),
            SideEffects {
                ap_change: None,
                local_writes: 9,
                resource_usages: ResourceMap::new(),
            }
        );
        assert_eq!(
            SideEffects {
                ap_change: None,
                local_writes: 3,
                resource_usages: ResourceMap::new(),
            }
            .converge(&SideEffects {
                ap_change: Some(5),
                local_writes: 1,
                resource_usages: ResourceMap::new(),
            }),
            SideEffects {
                ap_change: None,
                local_writes: 3,
                resource_usages: ResourceMap::new(),
            }
        );
        assert_eq!(
            SideEffects {
                ap_change: Some(2),
                local_writes: 9,
                resource_usages: ResourceMap::new(),
            }
            .converge(&SideEffects {
                ap_change: None,
                local_writes: 0,
                resource_usages: ResourceMap::new(),
            }),
            SideEffects {
                ap_change: None,
                local_writes: 9,
                resource_usages: ResourceMap::new(),
            }
        );
    }
}
