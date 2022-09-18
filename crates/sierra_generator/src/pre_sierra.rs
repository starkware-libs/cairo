use db_utils::define_short_id;
use defs::ids::FreeFunctionId;
use sierra::ids::ConcreteTypeId;
use sierra::program;
use utils::write_comma_separated;

use crate::db::SierraGenGroup;

/// Represents the long id of a pre-sierra label.
/// The long id consists of the parent function and a unique identifier inside the function.
// TODO(lior): Make sure this struct can only be constructed by expr_generator_context.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct LabelLongId {
    pub parent: FreeFunctionId,
    // A unique identifier inside the function
    pub id: usize,
}
define_short_id!(LabelId, LabelLongId, SierraGenGroup, lookup_intern_label_id);

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
    /// The parameters for the function.
    pub parameters: Vec<program::Param>,
    /// The return types from the function.
    pub ret_types: Vec<sierra::ids::ConcreteTypeId>,
}

/// Represents a pre-sierra statement - a statement before label-resolution.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Sierra(program::GenStatement<LabelId>),
    Label(Label),
    PushValues(Vec<PushValue>),
}
impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Sierra(value) => write!(f, "{}", value),
            Statement::Label(Label { id }) => write!(f, "{}:", id),
            Statement::PushValues(values) => {
                write!(f, "PushValues(")?;
                write_comma_separated(
                    f,
                    &values
                        .iter()
                        .map(|PushValue { var, ty, .. }| format!("{var}: {ty}"))
                        .collect::<Vec<String>>(),
                )?;
                write!(f, ") -> (")?;
                write_comma_separated(
                    f,
                    &values
                        .iter()
                        .map(|PushValue { var_on_stack, .. }| var_on_stack)
                        .collect::<Vec<_>>(),
                )?;
                write!(f, ")")
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PushValue {
    pub var: sierra::ids::VarId,
    pub var_on_stack: sierra::ids::VarId,
    pub ty: ConcreteTypeId,
}

/// Represents a pre-sierra label.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Label {
    pub id: LabelId,
}
