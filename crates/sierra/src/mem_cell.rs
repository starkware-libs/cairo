// The value of a single memory cell in Sierra.
#[derive(Debug, PartialEq)]
pub struct MemCell {
    // Temporarily i64 and not felt, for simplicity.
    pub value: i64,
}
impl From<i64> for MemCell {
    fn from(value: i64) -> Self {
        Self { value }
    }
}
