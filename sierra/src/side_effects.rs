use std::cmp::{max, min};

use crate::{
    context::{Context, Resource, ResourceMap},
    graph::ApChange::{self, *},
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SideEffects {
    pub ap_change: ApChange,
    pub local_writes: usize,
    pub resources: ResourceMap,
}

impl SideEffects {
    pub(crate) fn new(before: &Context, after: &Context) -> SideEffects {
        SideEffects {
            ap_change: if after.temp_invalidated {
                Unknown
            } else {
                Known(after.temp_cursur - before.temp_cursur)
            },
            local_writes: after.local_cursur - before.local_cursur,
            resources: Resource::iterator()
                .map(|r| {
                    (
                        *r,
                        after.resources.get(r).unwrap_or(&0)
                            - before.resources.get(r).unwrap_or(&0),
                    )
                })
                .collect(),
        }
    }

    pub(crate) fn add(self: &Self, other: &SideEffects) -> SideEffects {
        SideEffects {
            ap_change: match (&self.ap_change, &other.ap_change) {
                (Known(c1), Known(c2)) => Known(c1 + c2),
                _ => Unknown,
            },
            local_writes: self.local_writes + other.local_writes,
            resources: Resource::iterator()
                .map(|r| {
                    (
                        *r,
                        self.resources.get(r).unwrap_or(&0) + other.resources.get(r).unwrap_or(&0),
                    )
                })
                .collect(),
        }
    }

    pub(crate) fn converge(self: &Self, other: &SideEffects) -> SideEffects {
        SideEffects {
            ap_change: match (&self.ap_change, &other.ap_change) {
                (Known(c1), Known(c2)) if c1 == c2 => Known(*c1),
                _ => Unknown,
            },
            local_writes: max(self.local_writes, other.local_writes),
            resources: Resource::iterator()
                .map(|r| {
                    (
                        *r,
                        *min(
                            self.resources.get(r).unwrap_or(&0),
                            other.resources.get(r).unwrap_or(&0),
                        ),
                    )
                })
                .collect(),
        }
    }
}

#[test]
fn test_new() {
    assert_eq!(
        SideEffects::new(
            &Context {
                local_cursur: 3,
                local_allocated: false,
                temp_used: true,
                temp_cursur: 10,
                temp_invalidated: false,
                resources: ResourceMap::from([(Resource::Gas, 0)]),
            },
            &Context {
                local_cursur: 7,
                local_allocated: false,
                temp_used: true,
                temp_cursur: 21,
                temp_invalidated: false,
                resources: ResourceMap::from([(Resource::Gas, 0)]),
            }
        ),
        SideEffects {
            ap_change: Known(11),
            local_writes: 4,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
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
                resources: ResourceMap::from([(Resource::Gas, 0)]),
            },
            &Context {
                local_cursur: 8,
                local_allocated: false,
                temp_used: true,
                temp_cursur: 21,
                temp_invalidated: true,
                resources: ResourceMap::from([(Resource::Gas, 0)]),
            }
        ),
        SideEffects {
            ap_change: Unknown,
            local_writes: 1,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
    );
}

#[test]
fn test_add() {
    assert_eq!(
        SideEffects {
            ap_change: Known(2),
            local_writes: 1,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
        .add(&SideEffects {
            ap_change: Known(5),
            local_writes: 7,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }),
        SideEffects {
            ap_change: Known(7),
            local_writes: 8,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
    );
    assert_eq!(
        SideEffects {
            ap_change: Unknown,
            local_writes: 3,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
        .add(&SideEffects {
            ap_change: Known(5),
            local_writes: 1,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }),
        SideEffects {
            ap_change: Unknown,
            local_writes: 4,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
    );
    assert_eq!(
        SideEffects {
            ap_change: Known(2),
            local_writes: 9,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
        .add(&SideEffects {
            ap_change: Unknown,
            local_writes: 0,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }),
        SideEffects {
            ap_change: Unknown,
            local_writes: 9,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
    );
}

#[test]
fn test_converge() {
    assert_eq!(
        SideEffects {
            ap_change: Known(2),
            local_writes: 1,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
        .converge(&SideEffects {
            ap_change: Known(2),
            local_writes: 7,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }),
        SideEffects {
            ap_change: Known(2),
            local_writes: 7,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
    );
    assert_eq!(
        SideEffects {
            ap_change: Known(2),
            local_writes: 9,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
        .converge(&SideEffects {
            ap_change: Known(3),
            local_writes: 7,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }),
        SideEffects {
            ap_change: Unknown,
            local_writes: 9,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
    );
    assert_eq!(
        SideEffects {
            ap_change: Unknown,
            local_writes: 3,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
        .converge(&SideEffects {
            ap_change: Known(5),
            local_writes: 1,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }),
        SideEffects {
            ap_change: Unknown,
            local_writes: 3,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
    );
    assert_eq!(
        SideEffects {
            ap_change: Known(2),
            local_writes: 9,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
        .converge(&SideEffects {
            ap_change: Unknown,
            local_writes: 0,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }),
        SideEffects {
            ap_change: Unknown,
            local_writes: 9,
            resources: ResourceMap::from([(Resource::Gas, 0)]),
        }
    );
}
