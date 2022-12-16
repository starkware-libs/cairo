use std::collections::HashSet;
use std::sync::Arc;

use db_utils::Upcast;
use defs::ids::{FreeFunctionId, GenericFunctionId, GenericParamId, LanguageElementId};
use diagnostics::{Diagnostics, Maybe, ToMaybe};
use diagnostics_proc_macros::DebugWithDb;
use id_arena::Arena;
use syntax::node::ast;
use utils::try_extract_matches;
use utils::unordered_hash_map::UnorderedHashMap;

use super::attribute::{ast_attributes_to_semantic, Attribute};
use super::generics::semantic_generic_params;
use crate::corelib::never_ty;
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::expr::compute::{compute_expr_block_semantic, ComputationContext, Environment};
use crate::resolve_path::{ResolvedLookback, Resolver};
use crate::{semantic, Expr, ExprId, FunctionId, SemanticDiagnostic, TypeId};

#[cfg(test)]
#[path = "free_function_test.rs"]
mod test;

// === Declaration ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct FreeFunctionDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    signature: semantic::Signature,
    generic_params: Vec<GenericParamId>,
    environment: Environment,
    attributes: Vec<Attribute>,
    resolved_lookback: Arc<ResolvedLookback>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_diagnostics].
pub fn free_function_declaration_diagnostics(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_free_function_declaration_data(free_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_signature].
pub fn free_function_declaration_signature(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<semantic::Signature> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.signature)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_attributes].
pub fn free_function_declaration_attributes(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_implicits].
pub fn free_function_declaration_implicits(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Vec<TypeId>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.signature.implicits)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_generic_params].
pub fn free_function_declaration_generic_params(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Vec<GenericParamId>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_resolved_lookback].
pub fn free_function_declaration_resolved_lookback(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.resolved_lookback)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_free_function_declaration_data].
pub fn priv_free_function_declaration_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<FreeFunctionDeclarationData> {
    let module_file_id = free_function_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let module_data = db.module_data(module_file_id.0)?;
    let function_syntax = module_data.free_functions.get(&free_function_id).to_maybe()?;
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_file_id,
        &function_syntax.generic_params(db.upcast()),
    );
    let mut resolver = Resolver::new(db, module_file_id, &generic_params);
    let mut environment = Environment::default();

    let syntax_db = db.upcast();
    let signature_syntax = function_syntax.signature(syntax_db);
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        GenericFunctionId::Free(free_function_id),
        &mut environment,
    );

    let attributes = ast_attributes_to_semantic(syntax_db, function_syntax.attributes(syntax_db));
    let resolved_lookback = Arc::new(resolver.lookback);
    Ok(FreeFunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature,
        generic_params,
        environment,
        attributes,
        resolved_lookback,
    })
}

// === Definition ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct FreeFunctionDefinitionData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    expr_lookup: UnorderedHashMap<ast::ExprPtr, ExprId>,
    resolved_lookback: Arc<ResolvedLookback>,
    definition: Arc<FreeFunctionDefinition>,
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct FreeFunctionDefinition {
    pub exprs: Arena<semantic::Expr>,
    pub statements: Arena<semantic::Statement>,
    pub body: semantic::ExprId,
    /// The set of direct callees of the free function (user functions and libfuncs that are called
    /// from this free function). The items in the vector are unique.
    pub direct_callees: Vec<FunctionId>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::free_function_definition_diagnostics].
pub fn free_function_definition_diagnostics(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_free_function_definition_data(free_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::free_function_definition_body].
pub fn free_function_definition_body(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<semantic::ExprId> {
    Ok(db.priv_free_function_definition_data(free_function_id)?.definition.body)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_definition_direct_callees].
pub fn free_function_definition_direct_callees(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Vec<FunctionId>> {
    Ok(db.priv_free_function_definition_data(free_function_id)?.definition.direct_callees.clone())
}

/// Query implementation of
/// [crate::db::SemanticGroup::free_function_definition_direct_free_function_callees].
pub fn free_function_definition_direct_free_function_callees(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Vec<FreeFunctionId>> {
    Ok(db
        .free_function_definition_direct_callees(free_function_id)?
        .into_iter()
        .filter_map(|function_id| {
            match db.lookup_intern_function(function_id).function.generic_function {
                GenericFunctionId::Free(free_function) => Some(free_function),
                _ => None,
            }
        })
        .collect())
}

/// Query implementation of [crate::db::SemanticGroup::free_function_definition].
pub fn free_function_definition(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Arc<FreeFunctionDefinition>> {
    Ok(db.priv_free_function_definition_data(free_function_id)?.definition)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_definition_resolved_lookback].
pub fn free_function_definition_resolved_lookback(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db.priv_free_function_definition_data(free_function_id)?.resolved_lookback)
}

// ---Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_free_function_definition_data].
pub fn priv_free_function_definition_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<FreeFunctionDefinitionData> {
    let module_file_id = free_function_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let module_data = db.module_data(module_file_id.0)?;
    let syntax = module_data.free_functions.get(&free_function_id).to_maybe()?.clone();
    // Compute signature semantic.
    let declaration = db.priv_free_function_declaration_data(free_function_id)?;
    let resolver = Resolver::new(db, module_file_id, &declaration.generic_params);
    let environment = declaration.environment;
    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(
        db,
        &mut diagnostics,
        resolver,
        &declaration.signature,
        environment,
    );
    let expr = compute_expr_block_semantic(&mut ctx, &syntax.body(db.upcast()))?;
    let expr_ty = expr.ty();
    let signature_return_ty = declaration.signature.return_type;
    if expr_ty != signature_return_ty
        && !expr_ty.is_missing(db)
        && !signature_return_ty.is_missing(db)
        && expr_ty != never_ty(db)
    {
        ctx.diagnostics.report(
            &syntax.body(db.upcast()),
            SemanticDiagnosticKind::WrongReturnType {
                expected_ty: signature_return_ty,
                actual_ty: expr_ty,
            },
        );
    }
    let body = ctx.exprs.alloc(expr);
    let ComputationContext { exprs, statements, resolver, .. } = ctx;

    let direct_callees: HashSet<FunctionId> = exprs
        .iter()
        .filter_map(|(_id, expr)| try_extract_matches!(expr, Expr::FunctionCall))
        .map(|f| f.function)
        .collect();

    let expr_lookup: UnorderedHashMap<_, _> =
        exprs.iter().map(|(expr_id, expr)| (expr.stable_ptr(), expr_id)).collect();
    let resolved_lookback = Arc::new(resolver.lookback);
    Ok(FreeFunctionDefinitionData {
        diagnostics: diagnostics.build(),
        expr_lookup,
        resolved_lookback,
        definition: Arc::new(FreeFunctionDefinition {
            exprs,
            statements,
            body,
            direct_callees: direct_callees.into_iter().collect(),
        }),
    })
}

/// Query implementation of [crate::db::SemanticGroup::expr_semantic].
/// Assumes function and expression are present.
pub fn expr_semantic(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
    id: semantic::ExprId,
) -> semantic::Expr {
    db.priv_free_function_definition_data(free_function_id)
        .unwrap()
        .definition
        .exprs
        .get(id)
        .unwrap()
        .clone()
}

/// Query implementation of [crate::db::SemanticGroup::statement_semantic].
/// Assumes function and statement are valid.
pub fn statement_semantic(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
    id: semantic::StatementId,
) -> semantic::Statement {
    db.priv_free_function_definition_data(free_function_id)
        .unwrap()
        .definition
        .statements
        .get(id)
        .unwrap()
        .clone()
}

pub trait SemanticExprLookup<'a>: Upcast<dyn SemanticGroup + 'a> {
    fn lookup_expr_by_ptr(
        &self,
        free_function_id: FreeFunctionId,
        ptr: ast::ExprPtr,
    ) -> Maybe<ExprId> {
        let definition_data = self.upcast().priv_free_function_definition_data(free_function_id)?;
        definition_data.expr_lookup.get(&ptr).copied().to_maybe()
    }
}

impl<'a, T: Upcast<dyn SemanticGroup + 'a> + ?Sized> SemanticExprLookup<'a> for T {}
