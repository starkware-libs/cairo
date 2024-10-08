use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_diagnostics::DiagnosticAdded;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use id_arena::Arena;
use smol_str::SmolStr;

use super::fmt::ExprFormatter;
use crate::db::SemanticGroup;
use crate::{ConcreteStructId, ExprLiteral, ExprStringLiteral, LocalVariable, PatternId, semantic};

/// Semantic representation of a Pattern.
///
/// A pattern is a way to "destructure" values. A pattern may introduce new variables that are bound
/// to inner values of a specific value. For example, a tuple pattern destructures a tuple
/// and may result in new variables for an elements of that tuple.
/// This is used both in let statements and match statements.
// TODO(spapini): Replace this doc with a reference to the language documentation about patterns,
// once it is available.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub enum Pattern {
    Literal(PatternLiteral),
    StringLiteral(PatternStringLiteral),
    Variable(PatternVariable),
    Struct(PatternStruct),
    Tuple(PatternTuple),
    FixedSizeArray(PatternFixedSizeArray),
    EnumVariant(PatternEnumVariant),
    Otherwise(PatternOtherwise),
    Missing(PatternMissing),
}
impl Pattern {
    pub fn ty(&self) -> semantic::TypeId {
        match self {
            Pattern::Literal(literal) => literal.literal.ty,
            Pattern::StringLiteral(string_literal) => string_literal.string_literal.ty,
            Pattern::Variable(variable) => variable.var.ty,
            Pattern::Struct(pattern_struct) => pattern_struct.ty,
            Pattern::Tuple(pattern_tuple) => pattern_tuple.ty,
            Pattern::FixedSizeArray(pattern_fixed_size_array) => pattern_fixed_size_array.ty,
            Pattern::EnumVariant(pattern_enum_variant) => pattern_enum_variant.ty,
            Pattern::Otherwise(pattern_otherwise) => pattern_otherwise.ty,
            Pattern::Missing(pattern_missing) => pattern_missing.ty,
        }
    }

    pub fn variables(&self, queryable: &dyn PatternVariablesQueryable) -> Vec<PatternVariable> {
        match self {
            Pattern::Variable(variable) => vec![variable.clone()],
            Pattern::Struct(pattern_struct) => pattern_struct
                .field_patterns
                .iter()
                .flat_map(|(_member, pattern)| queryable.query(*pattern))
                .collect(),
            Pattern::Tuple(pattern_tuple) => pattern_tuple
                .field_patterns
                .iter()
                .flat_map(|pattern| queryable.query(*pattern))
                .collect(),
            Pattern::FixedSizeArray(pattern_fixed_size_array) => pattern_fixed_size_array
                .elements_patterns
                .iter()
                .flat_map(|pattern| queryable.query(*pattern))
                .collect(),
            Pattern::EnumVariant(pattern_enum_variant) => {
                match &pattern_enum_variant.inner_pattern {
                    Some(pattern) => queryable.query(*pattern),
                    None => vec![],
                }
            }
            Pattern::Literal(_)
            | Pattern::StringLiteral(_)
            | Pattern::Otherwise(_)
            | Pattern::Missing(_) => vec![],
        }
    }

    pub fn stable_ptr(&self) -> ast::PatternPtr {
        match self {
            Pattern::Literal(pattern) => pattern.stable_ptr,
            Pattern::StringLiteral(pattern) => pattern.stable_ptr,
            Pattern::Variable(pattern) => pattern.stable_ptr,
            Pattern::Struct(pattern) => pattern.stable_ptr.into(),
            Pattern::Tuple(pattern) => pattern.stable_ptr.into(),
            Pattern::FixedSizeArray(pattern) => pattern.stable_ptr.into(),
            Pattern::EnumVariant(pattern) => pattern.stable_ptr,
            Pattern::Otherwise(pattern) => pattern.stable_ptr.into(),
            Pattern::Missing(pattern) => pattern.stable_ptr,
        }
    }
}

impl From<&Pattern> for SyntaxStablePtrId {
    fn from(pattern: &Pattern) -> Self {
        pattern.stable_ptr().into()
    }
}

/// Polymorphic container of [`Pattern`] objects used for querying pattern variables.
pub trait PatternVariablesQueryable {
    /// Lookup the pattern in this container and then get [`Pattern::variables`] from it.
    fn query(&self, id: PatternId) -> Vec<PatternVariable>;
}

impl PatternVariablesQueryable for Arena<Pattern> {
    fn query(&self, id: PatternId) -> Vec<PatternVariable> {
        self[id].variables(self)
    }
}

/// Query a function for variables of patterns defined within it.
///
/// This is a wrapper over [`SemanticGroup`] that takes [`FunctionWithBodyId`]
/// and relays queries to [`SemanticGroup::pattern_semantic`].
pub struct QueryPatternVariablesFromDb<'a>(
    pub &'a (dyn SemanticGroup + 'static),
    pub FunctionWithBodyId,
);

impl PatternVariablesQueryable for QueryPatternVariablesFromDb<'_> {
    fn query(&self, id: PatternId) -> Vec<PatternVariable> {
        self.0.pattern_semantic(self.1, id).variables(self)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternLiteral {
    pub literal: ExprLiteral,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternStringLiteral {
    pub string_literal: ExprStringLiteral,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternPtr,
}

/// A pattern that binds the matched value to a variable.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct PatternVariable {
    #[dont_rewrite]
    pub name: SmolStr,
    pub var: LocalVariable,
    #[dont_rewrite]
    pub stable_ptr: ast::PatternPtr,
}
impl DebugWithDb<ExprFormatter<'_>> for PatternVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _db: &ExprFormatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// A pattern that destructures a struct to its fields.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternStruct {
    pub concrete_struct_id: ConcreteStructId,
    // TODO(spapini): This should be ConcreteMember, when available.
    pub field_patterns: Vec<(semantic::Member, PatternId)>,
    pub ty: semantic::TypeId,
    #[dont_rewrite]
    pub n_snapshots: usize,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternStructPtr,
}

/// A pattern that destructures a tuple to its fields.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternTuple {
    pub field_patterns: Vec<PatternId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternTuplePtr,
}

/// A pattern that destructures a fixed size array into its elements.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternFixedSizeArray {
    pub elements_patterns: Vec<PatternId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternFixedSizeArrayPtr,
}

/// A pattern that destructures a specific variant of an enum to its inner value.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternEnumVariant {
    pub variant: semantic::ConcreteVariant,
    pub inner_pattern: Option<PatternId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternOtherwise {
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::TerminalUnderscorePtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct PatternMissing {
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternPtr,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub diag_added: DiagnosticAdded,
}
