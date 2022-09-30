use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::{LocalVarId, StructId};
use diagnostics_proc_macros::DebugWithDb;
use smol_str::SmolStr;
use syntax::node::ast;

use super::fmt::ExprFormatter;
use crate::{semantic, ExprLiteral};

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub enum Pattern {
    Literal(ExprLiteral),
    Variable(PatternVariable),
    Struct(PatternStruct),
    Enum(PatternEnum),
    Otherwise,
}
impl Pattern {
    pub fn variables(&self) -> Vec<&PatternVariable> {
        match self {
            Pattern::Variable(variable) => vec![variable],
            Pattern::Struct(pattern_struct) => pattern_struct
                .field_patterns
                .iter()
                .flat_map(|(_member, pattern)| pattern.variables())
                .collect(),
            Pattern::Enum(pattern_enum) => pattern_enum.inner_pattern.variables(),
            Pattern::Literal(_) | Pattern::Otherwise => vec![],
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PatternVariable {
    pub name: SmolStr,
    pub var: LocalVariable,
}
impl DebugWithDb<ExprFormatter<'_>> for PatternVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _db: &ExprFormatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct LocalVariable {
    pub id: LocalVarId,
    pub ty: semantic::TypeId,
}
impl LocalVariable {
    pub fn stable_ptr(&self, db: &dyn DefsGroup) -> ast::TerminalIdentifierPtr {
        self.id.stable_ptr(db)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct PatternStruct {
    pub id: StructId,
    // TODO(spapini): This should be ConcreteMember, when available.
    pub field_patterns: Vec<(semantic::Member, Box<Pattern>)>,
    pub ty: semantic::TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct PatternEnum {
    pub variant: semantic::ConcreteVariant,
    pub inner_pattern: Box<Pattern>,
}
