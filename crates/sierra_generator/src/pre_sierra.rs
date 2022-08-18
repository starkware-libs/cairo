pub struct StatementId(usize);
impl From<usize> for StatementId {
    fn from(value: usize) -> Self {
        StatementId(value)
    }
}

pub struct Statement {
    pub statement_id: StatementId,
    pub statement: String,
}
