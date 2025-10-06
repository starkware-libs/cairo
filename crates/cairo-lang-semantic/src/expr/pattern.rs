use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_diagnostics::DiagnosticAdded;
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use salsa::Database;

use super::fmt::ExprFormatter;
use crate::items::function_with_body::FunctionWithBodySemantic;
use crate::{
    ConcreteStructId, ExprLiteral, ExprStringLiteral, LocalVariable, PatternArena, PatternId,
    semantic,
};

/// Semantic representation of a Pattern.
///
/// A pattern is a way to "destructure" values. A pattern may introduce new variables that are bound
/// to inner values of a specific value. For example, a tuple pattern destructures a tuple
/// and may result in new variables for an elements of that tuple.
/// This is used both in let statements and match statements.
// TODO(spapini): Replace this doc with a reference to the language documentation about patterns,
// once it is available.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub enum Pattern<'db> {
    Literal(PatternLiteral<'db>),
    StringLiteral(PatternStringLiteral<'db>),
    Variable(PatternVariable<'db>),
    Struct(PatternStruct<'db>),
    Tuple(PatternTuple<'db>),
    FixedSizeArray(PatternFixedSizeArray<'db>),
    EnumVariant(PatternEnumVariant<'db>),
    Otherwise(PatternOtherwise<'db>),
    Missing(PatternMissing<'db>),
}

impl<'db> Pattern<'db> {
    pub fn ty(&self) -> semantic::TypeId<'db> {
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

    pub fn variables(
        &self,
        queryable: &dyn PatternVariablesQueryable<'db>,
    ) -> Vec<PatternVariable<'db>> {
        match self {
            Pattern::Variable(variable) => vec![variable.clone()],
            Pattern::Struct(pattern_struct) => pattern_struct
                .field_patterns
                .iter()
                .flat_map(|(pattern, _member)| queryable.query(*pattern))
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

    pub fn stable_ptr(&self) -> ast::PatternPtr<'db> {
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

impl<'db> From<&Pattern<'db>> for SyntaxStablePtrId<'db> {
    fn from(pattern: &Pattern<'db>) -> Self {
        pattern.stable_ptr().into()
    }
}

/// Polymorphic container of [`Pattern`] objects used for querying pattern variables.
pub trait PatternVariablesQueryable<'a> {
    /// Lookup the pattern in this container and then get [`Pattern::variables`] from it.
    fn query(&self, id: PatternId) -> Vec<PatternVariable<'a>>;
}

impl<'a> PatternVariablesQueryable<'a> for PatternArena<'a> {
    fn query(&self, id: PatternId) -> Vec<PatternVariable<'a>> {
        self[id].variables(self)
    }
}

/// Query a function for variables of patterns defined within it.
///
/// This is a wrapper over [`Database`] that takes [`FunctionWithBodyId`]
/// and relays queries to [`FunctionWithBodySemantic::pattern_semantic`].
pub struct QueryPatternVariablesFromDb<'a>(
    pub &'a (dyn Database + 'static),
    pub FunctionWithBodyId<'a>,
);

impl<'a> PatternVariablesQueryable<'a> for QueryPatternVariablesFromDb<'a> {
    fn query(&self, id: PatternId) -> Vec<PatternVariable<'a>> {
        let pattern: Pattern<'a> = self.0.pattern_semantic(self.1, id);
        pattern.variables(self)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct PatternLiteral<'db> {
    pub literal: ExprLiteral<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct PatternStringLiteral<'db> {
    pub string_literal: ExprStringLiteral<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternPtr<'db>,
}

/// A pattern that binds the matched value to a variable.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct PatternVariable<'db> {
    #[dont_rewrite]
    pub name: SmolStrId<'db>,
    pub var: LocalVariable<'db>,
    #[dont_rewrite]
    pub stable_ptr: ast::PatternPtr<'db>,
}
impl<'db> DebugWithDb<'db> for PatternVariable<'db> {
    type Db = ExprFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db ExprFormatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.long(db.db))
    }
}

/// A pattern that destructures a struct to its fields.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct PatternStruct<'db> {
    pub concrete_struct_id: ConcreteStructId<'db>,
    // TODO(spapini): This should be ConcreteMember, when available.
    pub field_patterns: Vec<(PatternId, semantic::Member<'db>)>,
    pub ty: semantic::TypeId<'db>,
    #[dont_rewrite]
    pub n_snapshots: usize,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternStructPtr<'db>,
}

/// A pattern that destructures a tuple to its fields.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct PatternTuple<'db> {
    pub field_patterns: Vec<PatternId>,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternTuplePtr<'db>,
}

/// A pattern that destructures a fixed size array into its elements.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct PatternFixedSizeArray<'db> {
    pub elements_patterns: Vec<PatternId>,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternFixedSizeArrayPtr<'db>,
}

/// A pattern that destructures a specific variant of an enum to its inner value.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct PatternEnumVariant<'db> {
    pub variant: semantic::ConcreteVariant<'db>,
    pub inner_pattern: Option<PatternId>,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct PatternOtherwise<'db> {
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::TerminalUnderscorePtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct PatternMissing<'db> {
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::PatternPtr<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub diag_added: DiagnosticAdded,
}
