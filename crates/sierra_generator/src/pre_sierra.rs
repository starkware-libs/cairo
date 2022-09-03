use db_utils::define_short_id;
use defs::ids::FreeFunctionId;
use sierra::program;

// TODO(lior): Make sure this struct can only be constructed by expr_generator_context.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct LabelLongId {
    pub parent: FreeFunctionId,
    // A unique identifier inside the function
    pub id: usize,
}
define_short_id!(LabelId);

impl std::fmt::Display for LabelId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "label{}", self.0)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    pub body: Vec<Statement>,
    pub entry_point: LabelId,
    pub id: sierra::ids::FunctionId,
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
