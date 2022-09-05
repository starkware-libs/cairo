use db_utils::define_short_id;
use defs::ids::FreeFunctionId;
use sierra::program;

/// Represents the long id of a pre-sierra label.
/// The long id consists of the parent function and a unique identifier inside the function.
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

/// Represents a compiled function before the label-resolution phase (pre-sierra).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    /// The source function which was compiled.
    pub id: sierra::ids::FunctionId,
    /// The body of the function.
    pub body: Vec<Statement>,
    /// A label pointing to the first instruction of the function.
    pub entry_point: LabelId,
    pub parameters: Vec<program::Param>,
}

/// Represents a pre-sierra statement - a statement before label-resolution.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Sierra(program::GenStatement<LabelId>),
    Label(Label),
}
impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Sierra(value) => write!(f, "{}", value),
            Statement::Label(Label { id }) => write!(f, "{}:", id),
        }
    }
}

/// Represents a pre-sierra label.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Label {
    pub id: LabelId,
}
