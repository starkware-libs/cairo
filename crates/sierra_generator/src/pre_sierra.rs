use sierra::program;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct LabelId(usize);
impl LabelId {
    pub fn new(value: usize) -> Self {
        LabelId(value)
    }
}
impl std::fmt::Display for LabelId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "label{}", self.0)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    SierraStatement(program::GenStatement<LabelId>),
    Label(Label),
}
impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::SierraStatement(value) => write!(f, "{}", value),
            Statement::Label(Label { id }) => write!(f, "{}:", id),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Label {
    pub id: LabelId,
}
