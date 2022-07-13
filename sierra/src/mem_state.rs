#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    Temp(i64),
    Local(i64),
    Transient(Vec<Location>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemState {
    pub local_cursur: usize,
    pub local_allocated: bool,
    pub temp_used: bool,
    pub temp_cursur: usize,
    pub temp_invalidated: bool,
}
