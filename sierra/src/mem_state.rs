#[derive(Debug, Clone)]
pub enum ApChange {
    Unknown,
    Known(usize),
}

#[derive(Debug, Clone)]
pub enum Location {
    Temp(i64),
    Local(i64),
    Transient,
}

#[derive(Debug, Clone)]
pub struct MemState {
    pub temp_offset: usize,
    pub local_offset: usize,
    pub ap_change: ApChange,
}
