/// The value of a single memory cell in Sierra.
#[derive(Debug, Eq, PartialEq)]
pub struct MemCell {
    // TODO(oziv): make the value a felt, currently i64 for simplicity.
    pub value: i64,
}
impl From<i64> for MemCell {
    fn from(value: i64) -> Self {
        Self { value }
    }
}
