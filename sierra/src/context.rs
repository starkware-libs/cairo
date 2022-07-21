use std::collections::HashMap;

use crate::graph::Identifier;

// The context that can be changed by a running line, not including its direct inputs and outputs.
#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    pub local_cursur: usize,
    pub local_allocated: bool,
    pub temp_used: bool,
    pub temp_cursur: usize,
    pub temp_invalidated: bool,
    pub resources: ResourceMap,
}

pub(crate) type ResourceMap = HashMap<Identifier, i64>;
