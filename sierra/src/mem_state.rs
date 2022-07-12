#[derive(Debug, Clone, PartialEq)]
pub enum ApChange {
    Unknown,
    Known(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    Temp(i64),
    Local(i64),
    Transient(Vec<Location>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemState {
    pub temp_cursur: usize,
    pub local_cursur: usize,
    pub ap_change: ApChange,
}
