use std::hash::Hash;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_lowering::ids::{ConcreteFunctionWithBodyId, LocationId};
use cairo_lang_proc_macros::HeapSize;
use cairo_lang_sierra as sierra;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program;
use cairo_lang_utils::define_short_id;
use itertools::Itertools;
use salsa::Database;

/// Represents the long ID of a pre-Sierra label.
/// The long id consists of the parent function and a unique identifier inside the function.
// TODO(lior): Make sure this struct can only be constructed by expr_generator_context.
#[derive(Clone, Debug, Eq, PartialEq, Hash, salsa::SalsaValue, HeapSize)]
pub struct LabelLongId<'db> {
    pub parent: ConcreteFunctionWithBodyId<'db>,
    // A unique identifier inside the function
    pub id: usize,
}
define_short_id!(LabelId, LabelLongId<'db>);

pub struct LabelIdWithDb<'db> {
    db: &'db dyn Database,
    label_id: LabelId<'db>,
}
impl std::fmt::Display for LabelIdWithDb<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LabelLongId { parent, id } = self.label_id.long(self.db);
        let parent = parent.function_id(self.db).unwrap();
        let dbg = format!("{:?}", parent.debug(self.db));
        write!(f, "label_{dbg}::{id}")
    }
}

impl<'db> LabelId<'db> {
    pub fn with_db(&self, db: &'db dyn Database) -> LabelIdWithDb<'db> {
        LabelIdWithDb { db, label_id: *self }
    }
}

/// Represents a compiled function before the label-resolution phase (pre-Sierra).
#[derive(Clone, Debug, Eq, PartialEq, salsa::SalsaValue)]
pub struct Function<'db> {
    /// The source function which was compiled.
    pub id: sierra::ids::FunctionId,
    /// The body of the function.
    pub body: Vec<StatementWithLocation<'db>>,
    /// A label pointing to the first instruction of the function.
    pub entry_point: LabelId<'db>,
    /// The parameters for the function.
    pub parameters: Vec<program::Param>,
    /// The location per variable in the function.
    /// SAFETY: `sierra::ids::VarId` is a foreign `'static` type, so the pair cannot be checked
    /// structurally; both elements are safe to retain across revisions.
    #[salsa_value(unsafe(prove_safe_to_retain_manually))]
    pub variable_locations: Vec<(sierra::ids::VarId, LocationId<'db>)>,
    /// Location of the function signature.
    pub signature_location: LocationId<'db>,
}

/// Represents a pre-Sierra statement - a statement before label-resolution.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement<'db> {
    /// A compiled Sierra statement (before label resolution).
    Sierra(program::GenStatement<LabelId<'db>>),
    /// A label.
    Label(Label<'db>),
    /// An instruction to push variables onto the stack. For example, used before calling functions
    /// and returning.
    ///
    /// Note that push values does not guarantee that new copies of the values will be pushed.
    /// If a prefix of the values is already on the stack, they will not be re-pushed.
    PushValues(Vec<PushValue>),
}
impl<'db> Statement<'db> {
    pub fn into_statement_without_location(self) -> StatementWithLocation<'db> {
        StatementWithLocation { statement: self, location: None }
    }
    pub fn to_string(&self, db: &dyn Database) -> String {
        StatementWithDb { db, statement: self.clone() }.to_string()
    }
}

/// Represents a pre-Sierra statement, with its location in the source code.
#[derive(Clone, Debug, Eq, PartialEq, salsa::SalsaValue)]
pub struct StatementWithLocation<'db> {
    /// SAFETY: composed of interned ids and owned data, all safe to retain across revisions;
    /// `GenStatement` is a foreign type, so this cannot be derived.
    #[salsa_value(unsafe(prove_safe_to_retain_manually))]
    pub statement: Statement<'db>,
    pub location: Option<LocationId<'db>>,
}

impl<'db> StatementWithLocation<'db> {
    pub fn set_location(&mut self, location: Option<LocationId<'db>>) {
        self.location = location;
    }
}

struct StatementWithDb<'db> {
    db: &'db dyn Database,
    statement: Statement<'db>,
}
impl<'db> std::fmt::Display for StatementWithDb<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.statement {
            Statement::Sierra(value) => {
                write!(f, "{}", value.clone().map(|label_id| label_id.with_db(self.db)))
            }
            Statement::Label(Label { id }) => {
                write!(f, "{}:", id.with_db(self.db))
            }
            Statement::PushValues(values) => {
                write!(
                    f,
                    "PushValues({}) -> ({})",
                    values.iter().format_with(", ", |PushValue { var, ty, .. }, f| f(
                        &format_args!("{var}: {ty}")
                    )),
                    values.iter().format_with(", ", |PushValue { var_on_stack, dup, .. }, f| {
                        if *dup { f(&format_args!("{var_on_stack}*")) } else { f(var_on_stack) }
                    })
                )
            }
        }
    }
}

/// Represents a single element that should be pushed onto the stack as part of
/// [Statement::PushValues].
#[derive(Clone, Debug, Eq, PartialEq, salsa::SalsaValue)]
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

/// Represents a pre-Sierra label.
#[derive(Clone, Debug, Eq, PartialEq, salsa::SalsaValue)]
pub struct Label<'db> {
    pub id: LabelId<'db>,
}
