use std::collections::HashSet;
use std::sync::Arc;

use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::ast;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::Upcast;
use id_arena::Arena;

use super::attribute::Attribute;
use crate::db::SemanticGroup;
use crate::resolve_path::ResolvedLookback;
use crate::{semantic, ExprId, FunctionId, SemanticDiagnostic};

// === Declaration ===

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::function_declaration_diagnostics].
pub fn function_declaration_diagnostics(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Diagnostics<SemanticDiagnostic> {
    let declaration_data = match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            db.priv_free_function_declaration_data(free_function_id)
        }
        FunctionWithBodyId::Impl(impl_function_id) => db
            .priv_impl_function_declaration_data(impl_function_id)
            .map(|x| x.function_declaration_data),
    };
    declaration_data.map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::function_with_body_signature].
pub fn function_with_body_signature(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<semantic::Signature> {
    match function_id {
        FunctionWithBodyId::Free(free_function_id) => db.free_function_signature(free_function_id),
        FunctionWithBodyId::Impl(impl_function_id) => db.impl_function_signature(impl_function_id),
    }
}

/// Query implementation of
/// [crate::db::SemanticGroup::function_with_body_generic_params].
pub fn function_with_body_generic_params(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Vec<semantic::GenericParam>> {
    match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            db.free_function_generic_params(free_function_id)
        }
        FunctionWithBodyId::Impl(impl_function_id) => {
            db.impl_function_generic_params(impl_function_id)
        }
    }
}

/// Query implementation of [crate::db::SemanticGroup::function_with_body_attributes].
pub fn function_with_body_attributes(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Vec<Attribute>> {
    match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            Ok(db.priv_free_function_declaration_data(free_function_id)?.attributes)
        }
        FunctionWithBodyId::Impl(impl_function_id) => Ok(db
            .priv_impl_function_declaration_data(impl_function_id)?
            .function_declaration_data
            .attributes),
    }
}

// === Body ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct FunctionBodyData {
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub expr_lookup: UnorderedHashMap<ast::ExprPtr, ExprId>,
    pub resolved_lookback: Arc<ResolvedLookback>,
    pub body: Arc<FunctionBody>,
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct FunctionBody {
    pub exprs: Arena<semantic::Expr>,
    pub statements: Arena<semantic::Statement>,
    pub body_expr: semantic::ExprId,
    /// The set of direct callees of the function (user functions and libfuncs that are called
    /// from this function).
    pub direct_callees: HashSet<FunctionId>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::function_body_diagnostics].
pub fn function_body_diagnostics(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Diagnostics<SemanticDiagnostic> {
    let body_data = match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            db.priv_free_function_body_data(free_function_id)
        }
        FunctionWithBodyId::Impl(impl_function_id) => {
            db.priv_impl_function_body_data(impl_function_id)
        }
    };
    body_data.map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::function_body_expr].
pub fn function_body_expr(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<semantic::ExprId> {
    Ok(db.function_body(function_id)?.body_expr)
}

/// Query implementation of [crate::db::SemanticGroup::function_body].
pub fn function_body(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Arc<FunctionBody>> {
    match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            Ok(db.priv_free_function_body_data(free_function_id)?.body)
        }
        FunctionWithBodyId::Impl(impl_function_id) => {
            Ok(db.priv_impl_function_body_data(impl_function_id)?.body)
        }
    }
}

/// Query implementation of
/// [crate::db::SemanticGroup::function_with_body_direct_callees].
pub fn function_with_body_direct_callees(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<HashSet<FunctionId>> {
    let direct_callees = match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            db.priv_free_function_body_data(free_function_id)?.body.direct_callees.clone()
        }
        FunctionWithBodyId::Impl(impl_function_id) => {
            db.priv_impl_function_body_data(impl_function_id)?.body.direct_callees.clone()
        }
    };
    Ok(direct_callees)
}

/// Query implementation of
/// [crate::db::SemanticGroup::function_with_body_direct_function_with_body_callees].
pub fn function_with_body_direct_function_with_body_callees(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<HashSet<FunctionWithBodyId>> {
    Ok(db
        .function_with_body_direct_callees(function_id)?
        .into_iter()
        .map(|function_id| function_id.try_get_function_with_body_id(db))
        .collect::<Maybe<Option<Vec<_>>>>()?
        .into_iter()
        .flatten()
        .collect())
}

// =========================================================

/// Query implementation of [crate::db::SemanticGroup::expr_semantic].
pub fn expr_semantic(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
    id: semantic::ExprId,
) -> semantic::Expr {
    db.function_body(function_id).unwrap().exprs.get(id).unwrap().clone()
}

/// Query implementation of [crate::db::SemanticGroup::statement_semantic].
pub fn statement_semantic(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
    id: semantic::StatementId,
) -> semantic::Statement {
    db.function_body(function_id).unwrap().statements.get(id).unwrap().clone()
}

pub trait SemanticExprLookup<'a>: Upcast<dyn SemanticGroup + 'a> {
    fn lookup_expr_by_ptr(
        &self,
        function_id: FunctionWithBodyId,
        ptr: ast::ExprPtr,
    ) -> Maybe<ExprId> {
        let body_data = match function_id {
            FunctionWithBodyId::Free(free_function_id) => {
                self.upcast().priv_free_function_body_data(free_function_id)
            }
            FunctionWithBodyId::Impl(impl_function_id) => {
                self.upcast().priv_impl_function_body_data(impl_function_id)
            }
        };
        body_data?.expr_lookup.get(&ptr).copied().to_maybe()
    }
}
impl<'a, T: Upcast<dyn SemanticGroup + 'a> + ?Sized> SemanticExprLookup<'a> for T {}
