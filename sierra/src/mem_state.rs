#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MemState {
    pub local_cursur: usize,
    pub local_allocated: bool,
    pub temp_used: bool,
    pub temp_cursur: usize,
    pub temp_invalidated: bool,
}
