use std::collections::HashSet;
use std::sync::Arc;

use cairo_lang_defs::ids::{
    FreeFunctionId, FunctionSignatureId, GenericParamId, LanguageElementId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_utils::try_extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use super::attribute::ast_attributes_to_semantic;
use super::function_with_body::{FunctionBody, FunctionBodyData};
use super::functions::FunctionDeclarationData;
use super::generics::semantic_generic_params;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::{compute_root_expr, ComputationContext, Environment};
use crate::resolve_path::{ResolvedLookback, Resolver};
use crate::{semantic, Expr, FunctionId, SemanticDiagnostic, TypeId};

#[cfg(test)]
#[path = "free_function_test.rs"]
mod test;

// === Declaration ===

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

/// Query implementation of [crate::db::SemanticGroup::free_function_signature].
pub fn free_function_signature(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<semantic::Signature> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.signature)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_implicits].
pub fn free_function_declaration_implicits(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Vec<TypeId>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.signature.implicits)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_generic_params].
pub fn free_function_generic_params(
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
) -> Maybe<FunctionDeclarationData> {
    let syntax_db = db.upcast();
    let module_file_id = free_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let module_free_functions = db.module_free_functions(module_file_id.0)?;
    let function_syntax = module_free_functions.get(&free_function_id).to_maybe()?;
    let declaration = function_syntax.declaration(syntax_db);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_file_id,
        &declaration.generic_params(syntax_db),
    );
    let mut resolver = Resolver::new(db, module_file_id, &generic_params);
    let mut environment = Environment::default();

    let signature_syntax = declaration.signature(syntax_db);
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        FunctionSignatureId::Free(free_function_id),
        &mut environment,
    );

    Ok(FunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature,
        environment,
        generic_params,
        attributes: ast_attributes_to_semantic(syntax_db, function_syntax.attributes(syntax_db)),
        resolved_lookback: Arc::new(resolver.lookback),
    })
}

// === Body ===

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::free_function_body_diagnostics].
pub fn free_function_body_diagnostics(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_free_function_body_data(free_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::free_function_body_resolved_lookback].
pub fn free_function_body_resolved_lookback(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db.priv_free_function_body_data(free_function_id)?.resolved_lookback)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_free_function_body_data].
pub fn priv_free_function_body_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<FunctionBodyData> {
    let module_file_id = free_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let module_free_functions = db.module_free_functions(module_file_id.0)?;
    let function_syntax = module_free_functions.get(&free_function_id).to_maybe()?.clone();
    // Compute declaration semantic.
    let declaration = db.priv_free_function_declaration_data(free_function_id)?;
    let resolver = Resolver::new(db, module_file_id, &declaration.generic_params);
    let environment = declaration.environment;
    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(
        db,
        &mut diagnostics,
        resolver,
        Some(&declaration.signature),
        environment,
    );
    let function_body = function_syntax.body(db.upcast());
    let return_type = declaration.signature.return_type;
    let expr = compute_root_expr(&mut ctx, &function_body, return_type)?;
    let body_expr = ctx.exprs.alloc(expr);
    let ComputationContext { exprs, statements, resolver, .. } = ctx;

    let direct_callees: HashSet<FunctionId> = exprs
        .iter()
        .filter_map(|(_id, expr)| try_extract_matches!(expr, Expr::FunctionCall))
        .map(|f| f.function)
        .collect();

    let expr_lookup: UnorderedHashMap<_, _> =
        exprs.iter().map(|(expr_id, expr)| (expr.stable_ptr(), expr_id)).collect();
    let resolved_lookback = Arc::new(resolver.lookback);
    Ok(FunctionBodyData {
        diagnostics: diagnostics.build(),
        expr_lookup,
        resolved_lookback,
        body: Arc::new(FunctionBody {
            exprs,
            statements,
            body_expr,
            direct_callees: direct_callees.into_iter().collect(),
        }),
    })
}
