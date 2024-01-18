use std::collections::HashMap;
use std::hash::Hash;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra as sierra;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program;
use cairo_lang_utils::{define_short_id, write_comma_separated};
use sierra::program::StatementIdx;

use crate::db::SierraGenGroup;

/// Represents the long id of a pre-sierra label.
/// The long id consists of the parent function and a unique identifier inside the function.
// TODO(lior): Make sure this struct can only be constructed by expr_generator_context.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct LabelLongId {
    pub parent: ConcreteFunctionWithBodyId,
    // A unique identifier inside the function
    pub id: usize,
}
define_short_id!(LabelId, LabelLongId, SierraGenGroup, lookup_intern_label_id);

pub struct LabelIdWithDb<'db> {
    db: &'db dyn SierraGenGroup,
    label_id: LabelId,
}
impl<'db> std::fmt::Display for LabelIdWithDb<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LabelLongId { parent, id } = self.db.lookup_intern_label_id(self.label_id);
        let parent = parent.function_id(self.db.upcast()).unwrap();
        let dbg = format!("{:?}", parent.debug(self.db));
        write!(f, "label_{}::{}", dbg, id)
    }
}

impl LabelId {
    pub fn with_db<'db>(&self, db: &'db dyn SierraGenGroup) -> LabelIdWithDb<'db> {
        LabelIdWithDb { db, label_id: *self }
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
pub enum StatementInner {
    /// A compiled Sierra statement (before label resolution).
    Sierra(program::GenStatement<LabelId>),
    /// A label.
    Label(Label),
    /// An instruction to push variables onto the stack. For example, used before calling functions
    /// and returning.
    ///
    /// Note that push values does not guarantee that new copies of the values will be pushed.
    /// If a prefix of the values is already on the stack, they will not be re-pushed.
    PushValues(Vec<PushValue>),
}
impl StatementInner {
    pub fn into_statement_without_location(self) -> Statement {
        Statement { statement: self, location: None }
    }
    pub fn to_string(&self, db: &dyn SierraGenGroup) -> String {
        StatementWithDb { db, statement: self.clone() }.to_string()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Statement {
    pub statement: StatementInner,
    pub location: Option<StableLocation>,
}

impl Statement {
    pub fn set_location(&mut self, location: Option<StableLocation>) {
        self.location = location;
    }
}

/// The location of the high level source code which caused a statement to be generated.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StatementsLocations {
    pub locations: HashMap<StatementIdx, StableLocation>,
}

impl StatementsLocations {
    /// Creates a new [StatementsLocations] object from a list of statements.
    pub fn from_statements(statements: &[Statement]) -> Self {
        let mut locations = HashMap::new();
        for (idx, statement) in statements.iter().enumerate() {
            if let Some(location) = &statement.location {
                locations.insert(StatementIdx(idx), *location);
            }
        }
        Self { locations }
    }
}

struct StatementWithDb<'db> {
    db: &'db dyn SierraGenGroup,
    statement: StatementInner,
}
impl<'db> std::fmt::Display for StatementWithDb<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.statement {
            StatementInner::Sierra(value) => {
                write!(f, "{}", value.clone().map(|label_id| label_id.with_db(self.db)))
            }
            StatementInner::Label(Label { id }) => {
                write!(f, "{}:", id.with_db(self.db))
            }
            StatementInner::PushValues(values) => {
                write!(f, "PushValues(")?;
                write_comma_separated(
                    f,
                    values.iter().map(|PushValue { var, ty, .. }| format!("{var}: {ty}")),
                )?;
                write!(f, ") -> (")?;
                write_comma_separated(
                    f,
                    values.iter().map(|PushValue { var_on_stack, dup, .. }| {
                        if *dup { format!("{var_on_stack}*") } else { format!("{var_on_stack}") }
                    }),
                )?;
                write!(f, ")")
            }
        }
    }
}

/// Represents a single element that should be pushed onto the stack as part of
/// [StatementInner::PushValues].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PushValue {
    /// The variable id to push.
    pub var: sierra::ids::VarId,
    /// The variable id on the stack (e.g., the result of `store_temp()`).
    /// If `dup` is true, this variable cannot be `var`.
    pub var_on_stack: sierra::ids::VarId,
    /// The type of the variable.
    pub ty: ConcreteTypeId,
    /// Indicates whether the variable should be duplicated before it is pushed.
    pub dup: bool,
}

/// Represents a pre-sierra label.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Label {
    pub id: LabelId,
}
