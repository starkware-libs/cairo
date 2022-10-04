use std::sync::Arc;

use db_utils::Upcast;
use defs::ids::{FreeFunctionId, GenericParamId, LanguageElementId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;
use id_arena::Arena;
use syntax::node::ast;
use utils::unordered_hash_map::UnorderedHashMap;

use super::functions::{function_signature_params, function_signature_return_type};
use super::generics::semantic_generic_params;
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::expr::compute::{compute_expr_semantic, ComputationContext, Environment};
use crate::resolve_path::{ResolvedGenericItem, ResolvedLookback, Resolver};
use crate::{semantic, ExprId, SemanticDiagnostic};

#[cfg(test)]
#[path = "free_function_test.rs"]
mod test;

// Declaration.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct FreeFunctionDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    signature: semantic::Signature,
    generic_params: Vec<GenericParamId>,
    environment: Environment,
}

// Selectors.
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
) -> Option<semantic::Signature> {
    Some(db.priv_free_function_declaration_data(free_function_id)?.signature)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_generic_params].
pub fn free_function_declaration_generic_params(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<Vec<GenericParamId>> {
    Some(db.priv_free_function_declaration_data(free_function_id)?.generic_params)
}

// Computation.
/// Query implementation of [crate::db::SemanticGroup::priv_free_function_declaration_data].
pub fn priv_free_function_declaration_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<FreeFunctionDeclarationData> {
    let module_id = free_function_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    let module_data = db.module_data(module_id)?;
    let function_syntax = module_data.free_functions.get(&free_function_id)?;
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_id,
        &function_syntax.generic_params(db.upcast()),
    );
    let mut resolver = Resolver::new(db, module_id, &generic_params);
    let signature_syntax = function_syntax.signature(db.upcast());
    let return_type =
        function_signature_return_type(&mut diagnostics, db, &mut resolver, &signature_syntax);
    let (params, environment) =
        function_signature_params(&mut diagnostics, db, &mut resolver, &signature_syntax);
    Some(FreeFunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature: semantic::Signature { params, return_type },
        generic_params,
        environment,
    })
}

// Definition.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct FreeFunctionDefinitionData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    expr_lookup: UnorderedHashMap<ast::ExprPtr, ExprId>,
    resolved_lookback: ResolvedLookback,
    definition: Arc<FreeFunctionDefinition>,
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct FreeFunctionDefinition {
    pub exprs: Arena<semantic::Expr>,
    pub statements: Arena<semantic::Statement>,
    pub body: semantic::ExprId,
}

// Selectors.
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
) -> Option<semantic::ExprId> {
    Some(db.priv_free_function_definition_data(free_function_id)?.definition.body)
}
/// Query implementation of [crate::db::SemanticGroup::free_function_definition].
pub fn free_function_definition(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<Arc<FreeFunctionDefinition>> {
    Some(db.priv_free_function_definition_data(free_function_id)?.definition)
}

// Computation.
/// Query implementation of [crate::db::SemanticGroup::priv_free_function_definition_data].
pub fn priv_free_function_definition_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<FreeFunctionDefinitionData> {
    let module_id = free_function_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    let module_data = db.module_data(module_id)?;
    let syntax = module_data.free_functions.get(&free_function_id)?.clone();
    // Compute signature semantic.
    let declaration = db.priv_free_function_declaration_data(free_function_id)?;
    let resolver = Resolver::new(db, module_id, &declaration.generic_params);
    let environment = declaration.environment;
    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(
        db,
        &mut diagnostics,
        resolver,
        declaration.signature.return_type,
        environment,
    );
    let expr = compute_expr_semantic(&mut ctx, &ast::Expr::Block(syntax.body(db.upcast())));
    if expr.ty() != declaration.signature.return_type && expr.ty() != semantic::TypeId::missing(db)
    {
        ctx.diagnostics.report(
            &syntax.body(db.upcast()),
            SemanticDiagnosticKind::WrongReturnType {
                expected_ty: declaration.signature.return_type,
                actual_ty: expr.ty(),
            },
        );
    }
    let body = ctx.exprs.alloc(expr);
    let ComputationContext { exprs, statements, resolver, .. } = ctx;

    let expr_lookup: UnorderedHashMap<_, _> =
        exprs.iter().map(|(expr_id, expr)| (expr.stable_ptr(), expr_id)).collect();
    let resolved_lookback = resolver.lookback;
    Some(FreeFunctionDefinitionData {
        diagnostics: diagnostics.build(),
        expr_lookup,
        resolved_lookback,
        definition: Arc::new(FreeFunctionDefinition { exprs, statements, body }),
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
    ) -> Option<ExprId> {
        let definition_data = self.upcast().priv_free_function_definition_data(free_function_id)?;
        definition_data.expr_lookup.get(&ptr).copied()
    }
    fn lookup_resolved_generic_item_by_ptr(
        &self,
        free_function_id: FreeFunctionId,
        ptr: ast::TerminalIdentifierPtr,
    ) -> Option<ResolvedGenericItem> {
        let definition_data = self.upcast().priv_free_function_definition_data(free_function_id)?;
        definition_data.resolved_lookback.generic.get(&ptr).cloned()
    }
}

impl<'a, T: Upcast<dyn SemanticGroup + 'a> + ?Sized> SemanticExprLookup<'a> for T {}
