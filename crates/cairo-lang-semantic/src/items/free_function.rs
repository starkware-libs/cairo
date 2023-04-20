use std::sync::Arc;

use cairo_lang_defs::ids::{FreeFunctionId, FunctionTitleId, LanguageElementId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_syntax::attribute::structured::AttributeListStructurize;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use super::function_with_body::{get_inline_config, FunctionBody, FunctionBodyData};
use super::functions::{
    forbid_inline_always_with_impl_generic_param, FunctionDeclarationData, InlineConfiguration,
};
use super::generics::semantic_generic_params;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::{compute_root_expr, ComputationContext, Environment};
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::{semantic, SemanticDiagnostic, TypeId};

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
) -> Maybe<Vec<semantic::GenericParam>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_resolver_data].
pub fn free_function_declaration_resolver_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_inline_config].
pub fn free_function_declaration_inline_config(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<InlineConfiguration> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.inline_config)
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

    // Generic params.
    let mut resolver = Resolver::new(db, module_file_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &declaration.generic_params(syntax_db),
    )?;

    let mut environment = Environment::default();

    let signature_syntax = declaration.signature(syntax_db);
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        FunctionTitleId::Free(free_function_id),
        &mut environment,
    );

    let attributes = function_syntax.attributes(syntax_db).structurize(syntax_db);

    let inline_config = get_inline_config(db, &mut diagnostics, &attributes)?;

    forbid_inline_always_with_impl_generic_param(&mut diagnostics, &generic_params, &inline_config);

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = resolver.inference().finalize() {
        inference_err.report(&mut diagnostics, stable_ptr);
    }
    let generic_params = resolver
        .inference()
        .rewrite(generic_params)
        .map_err(|err| err.report(&mut diagnostics, function_syntax.stable_ptr().untyped()))?;
    let signature = resolver
        .inference()
        .rewrite(signature)
        .map_err(|err| err.report(&mut diagnostics, function_syntax.stable_ptr().untyped()))?;

    Ok(FunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature,
        environment,
        generic_params,
        attributes,
        resolver_data: Arc::new(resolver.data),
        inline_config,
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

/// Query implementation of [crate::db::SemanticGroup::free_function_body_resolver_data].
pub fn free_function_body_resolver_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_free_function_body_data(free_function_id)?.resolver_data)
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

    // Generic params.
    let mut resolver = Resolver::new(db, module_file_id);
    for generic_param in declaration.generic_params {
        resolver.add_generic_param(generic_param);
    }

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
    let body_expr = compute_root_expr(&mut ctx, &function_body, return_type)?;
    let ComputationContext { exprs, statements, resolver, .. } = ctx;

    let expr_lookup: UnorderedHashMap<_, _> =
        exprs.iter().map(|(expr_id, expr)| (expr.stable_ptr(), expr_id)).collect();
    let resolver_data = Arc::new(resolver.data);
    Ok(FunctionBodyData {
        diagnostics: diagnostics.build(),
        expr_lookup,
        resolver_data,
        body: Arc::new(FunctionBody { exprs, statements, body_expr }),
    })
}
