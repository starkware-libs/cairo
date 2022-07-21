use std::collections::HashMap;

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

pub(crate) type ResourceMap = HashMap<Resource, i64>;

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum Resource {
    Gas,
}

impl Resource {
    pub fn iterator() -> std::slice::Iter<'static, Resource> {
        static RESOURCES: [Resource; 1] = [Resource::Gas];
        RESOURCES.iter()
    }
}
