use debug::DebugWithDb;
use defs::ids::StructId;
use diagnostics_proc_macros::DebugWithDb;
use smol_str::SmolStr;

use super::fmt::ExprFormatter;
use crate::corelib::core_felt_ty;
use crate::db::SemanticGroup;
use crate::{semantic, ExprLiteral, LocalVariable};

/// Semantic representation of a Pattern.
/// A pattern is a way to "destructure" values. A pattern may introduce new variables that are bound
/// to inner values of a specific value. For example, a tuple pattern destructures a tuple
/// and may result in new variables for an elements of that tuple.
/// This is used both in let statements and match statements.
// TODO(spapini): Replace this doc with a reference to the language documentation about patterns,
// once it is available.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub enum Pattern {
    Literal(PatternLiteral),
    Variable(PatternVariable),
    Struct(PatternStruct),
    Tuple(PatternTuple),
    EnumVariant(PatternEnumVariant),
    Otherwise(PatternOtherwise),
}
impl Pattern {
    pub fn ty(&self, db: &dyn SemanticGroup) -> semantic::TypeId {
        match self {
            Pattern::Literal(_) => core_felt_ty(db),
            Pattern::Variable(variable) => variable.var.ty,
            Pattern::Struct(pattern_struct) => pattern_struct.ty,
            Pattern::Tuple(pattern_tuple) => pattern_tuple.ty,
            Pattern::EnumVariant(pattern_enum_variant) => pattern_enum_variant.ty,
            Pattern::Otherwise(pattern_otherwise) => pattern_otherwise.ty,
        }
    }

    pub fn variables(&self) -> Vec<&PatternVariable> {
        match self {
            Pattern::Variable(variable) => vec![variable],
            Pattern::Struct(pattern_struct) => pattern_struct
                .field_patterns
                .iter()
                .flat_map(|(_member, pattern)| pattern.variables())
                .collect(),
            Pattern::Tuple(pattern_tuple) => pattern_tuple
                .field_patterns
                .iter()
                .flat_map(|pattern| pattern.variables())
                .collect(),
            Pattern::EnumVariant(pattern_enum_variant) => {
                pattern_enum_variant.inner_pattern.variables()
            }
            Pattern::Literal(_) | Pattern::Otherwise(_) => vec![],
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternLiteral {
    pub literal: ExprLiteral,
    pub ty: semantic::TypeId,
}

/// A pattern that binds the matched value to a variable.
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

/// A pattern that destructures a struct to its fields.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternStruct {
    pub id: StructId,
    // TODO(spapini): This should be ConcreteMember, when available.
    pub field_patterns: Vec<(semantic::Member, Box<Pattern>)>,
    pub ty: semantic::TypeId,
}

/// A pattern that destructures a tuple to its fields.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternTuple {
    pub field_patterns: Vec<Box<Pattern>>,
    pub ty: semantic::TypeId,
}

/// A pattern that destructures a specific variant of an enum to its inner value.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternEnumVariant {
    pub variant: semantic::ConcreteVariant,
    pub inner_pattern: Box<Pattern>,
    pub ty: semantic::TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternOtherwise {
    pub ty: semantic::TypeId,
}
